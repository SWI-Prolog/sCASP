:- use_module(library(scasp)).
:- use_module(library(scasp/html)).
:- use_module(library(scasp/output)).

:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(http/jquery)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/term_html)).

% Become Unix daemon process when on Unix and not attached to a
% terminal.

:- if((current_prolog_flag(unix,true),
       \+ stream_property(current_input, tty(true)))).

:- format(user_error, '% Loading as Unix daemon~n', []).
:- use_module(library(http/http_unix_daemon)).

:- else.

:- initialization(main,main).

main(Argv) :-
    argv_options(Argv, _, Options),
    server(Options),
    thread_get_message(quit). % do not terminate

opt_type(port, port, natural).
opt_type(p,    port, natural).

opt_help(port, "Port used by the server").

opt_meta(port, 'PORT').


%!  server(+Options)
%
%   Start HTTP server at the indicated port.

server(Options) :-
    (   option(port(_), Options)
    ->  http_server(Options)
    ;   http_server([port(8080)|Options])
    ).
:- endif.

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(scasp, root(scasp), []).
http:location(js,    scasp(js),   []).
http:location(css,   scasp(css),  []).

:- http_handler(root(.),      http_redirect(see_other, scasp(.)), []).
:- http_handler(scasp(.),     home,  []).
:- http_handler(scasp(solve), solve, [id(solve)]).

home(_Request) :-
    reply_html_page([ title('s(CASP) web server')
                    ],
                    [ h1('s(CASP) web server'),
                      \query_page
                    ]).

query_page -->
    html_requires(jquery),
    html_requires(scasp),
    styles,
    html([ h4('Program'),
           textarea([id(data), rows(10), cols(80),
                     placeholder('s(CASP) program')], ''),
           h4('Query'),
           '?- ',     input([id(query),
                             placeholder('Query')]),
           ' Limit ', input([type(number), min(1), id(limit), value(1),
                             placeholder('Empty means all answer sets')]),
           \html_json_radio,
           h4(''),
           button(id(solve), 'Solve'),
           button(id(clear), 'Clear'),
           div(id(results), [])
         ]),
    button_actions.

styles -->
    html({|html||
<style>
div.html-json-switch { margin-top: 10px; margin-left: 2ex; }
</style>
         |}).


html_json_radio -->
    html(div(class('html-json-switch'),
             [ 'Display results as: ',
               input([type(radio), id(rhtml), name(format), value(html),
                      checked(checked)]),
               label(for(rhtml), 'HTML'),
               input([type(radio), id(rjson), name(format), value(json)]),
               label(for(rjson), 'JSON')
             ])).


button_actions -->
    { http_link_to_id(solve, [], SolveURL) },
    js_script({|javascript(SolveURL)||
$(function() {

$("#solve").on("click", function() {
  var data = $("#data").val();
  var query = $("#query").val();
  var limit = $("#limit").val();
  var format = $('input[type="radio"][name="format"]:checked').val();
  $("#results").empty();
  $.get(SolveURL,
        { data: data,
          query: query,
          limit: limit,
          format: format
        },
        function(reply) {
          var results = $("#results");

          results.html(reply);
          results.sCASP('swish_answer');
        })
  .fail(function(error) {
    if ( error.responseJSON ) {
      $("#results").text(error.responseJSON.message);
    } else {
      console.log(error);
    }
  });
});

$("#clear").on("click", function() {
  $("#results").empty();
});

});
              |}).


solve(Request) :-
    http_parameters(Request,
                    [ data(Data, []),
                      query(QueryS, []),
                      limit(Limit, [optional(true), integer]),
                      format(Format, [default(html)])
                    ]),
    Error = error(Formal,_),
    catch(( setup_call_cleanup(
                open_string(Data, In),
                read_terms(In, Terms),
                close(In)),
            term_string(Query, QueryS, [variable_names(VNames)])
          ),
          Error,
          true),
    (   var(Limit)
    ->  Limit = infinite
    ;   true
    ),
    (   nonvar(Formal)
    ->  reply_html_page([],
                        \error(Error))
    ;   in_temporary_module(
            M,
            ( use_module(library(scasp)),
              maplist(add_to_program(M), Terms)
            ),
            ( call_time(findall(Result,
                                scasp_result(M:Query, VNames, Result,
                                             [tree(true)]),
                                Results),
                        TotalTime),
              reply(Format, M:Results, TotalTime)
            ))
    ).

error(Error) -->
    { message_to_string(Error, Message) },
    html(div(class(error), Message)).

%!  add_to_program(+Module, +Term)
%
%   Add clauses to the program.  Also handles s(CASP) directives.

add_to_program(M, (# Directive)) =>
    #(M:Directive).
add_to_program(M, (:- show Spec)) =>
    show(M:Spec).
add_to_program(M, (:- pred Spec)) =>
    pred(M:Spec).
add_to_program(M, (:- abducible Spec)) =>
    abducible(M:Spec).
add_to_program(M, Term) =>
    scasp_assert(M:Term).

scasp_result(Query,
             Bindings,
             scasp{ query:Query,
                    answer:Counter,
                    bindings:Bindings,
                    model:Model,
                    tree:Tree,
                    time:Time
                  },
             Options) :-
    option(tree(true), Options),
    !,
    Query = M:_,
    call_nth(call_time(scasp(Query), Time), Counter),
    scasp_model(M:Model),
    scasp_justification(M:Tree, []),
    analyze_variables(t(Bindings, Model, Tree), Bindings, Options).

analyze_variables(Term, Bindings, Options) :-
    ovar_set_bindings(Bindings),
    (   option(inline_constraints(false), Options)
    ->  ovar_analyze_term(Term, [name_constraints(true)])
    ;   ovar_analyze_term(Term, []),
        inline_constraints(Term, [])
    ).

%!  reply(+Format, :Results, +TotalTime)

reply(html, M:Results, TotalTime) =>
    reply_html_page([],
                    \results(Results, M, TotalTime)).


		 /*******************************
		 *        HTML GENERATION	*
		 *******************************/

results([], _, Time) -->
    !,
    html(h3('No models (~3f sec)'-[Time.cpu])).
results(Results, M, _Time) -->
    sequence(result(M), Results).

result(M, Result) -->
    html(div(class(result),
             [ h3('Result #~D (~3f sec)'-[Result.answer, Result.time.cpu]),
               \binding_section(Result.bindings),
               \html_model(M:Result.model, [class('collapsable-content')]),
               \html_justification_tree(M:Result.tree, [])
             ])).

read_terms(In, Terms) :-
    read_one_term(In, Term0),
    read_terms(Term0, In, Terms).

read_terms(end_of_file, _, []) :-
    !.
read_terms(Term, In, [Term|T]) :-
    read_one_term(In, Term1),
    read_terms(Term1, In, T).

read_one_term(In, Term) :-
    read_term(In, Term,
              [ module(scasp_dyncall),
                variable_names(Bindings)
              ]),
    fixup_pred(Term, Bindings).

%!  fixup_pred(+Term, +Bindings) is det.
%
%   If Term is a `pred` directive, bind the variables in the atom
%   to '$VAR'(Name) if possible.

fixup_pred((#pred Atom :: _Template), Bindings) =>
    fixup_atom(Atom, Bindings).
fixup_pred((:-pred Atom :: _Template), Bindings) =>
    fixup_atom(Atom, Bindings).
fixup_pred(_, _) =>
    true.

fixup_atom(Var, Bindings), var(Var) =>
    (   member(Name = V, Bindings),
        V == Var
    ->  Var = '$VAR'(Name)
    ;   true
    ).
fixup_atom(Term, Bindings), compound(Term) =>
    compound_name_arguments(Term, _Name, Args),
    maplist(fixup_atom_r(Bindings), Args).
fixup_atom(_, _) =>
    true.

fixup_atom_r(Bindings, Atom) :-
    fixup_atom(Atom, Bindings).

%!  binding_section(+Bindings)//

binding_section([]) -->
    !.
binding_section(Bindings) -->
    { copy_term(Bindings, Bindings1, Constraints0),
      var_names(Constraints0, Constraints1),
      exclude(no_op_binding, Bindings1, Bindings2)
    },
    html(div(class(bindings),
             [ h4('Bindings'),
               \bindings(Bindings2, Constraints1)
             ])).

var_names([], []).
var_names([H|T0], T) :-
    var_name(H),
    !,
    var_names(T0, T).
var_names([H|T0], [H|T]) :-
    var_names(T0, T).

var_name(put_attr(Var, scasp_output, name(Name))) :-
    Var = '$VAR'(Name).
var_name(put_attr(Var, scasp_output, singleton)) :-
    Var = '$VAR'('_').

no_op_binding(Name = '$VAR'(Name)) => true.
no_op_binding(_) => false.


%!  bindings(+Bindings, +Constraints)//
%
%   Report on the bindings.

bindings([], []) -->
    !.
bindings([], Constraints) -->
    !,
    constraints(Constraints).
bindings([H|T], Constraints) -->
    (   {T == [], Constraints == []}
    ->  binding(H, '')
    ;   binding(H, ',')
    ),
    bindings(T, Constraints).

binding(Name=Value, End) -->
    html(div(class(binding),
             [ var(Name),
               ' = ',
               \term(Value),
               End
             ])).

constraints([]) -->
    [].
constraints([H|T]) -->
    (   {T==[]}
    ->  constraint(H, '')
    ;   constraint(H, ','),
        constraints(T)
    ).

constraint(H, End) -->
    html(div(class(constraint),
             [ \constraint(H),
               End
             ])).

constraint('\u2209'(V,S)) -->
    !,
    html([ \term(V), ' \u2209 ', \term(S) ]).
constraint({ClpQ}) -->
    !,
    { comma_list(ClpQ, List),
      maplist(to_clpq, List, Constraints)
    },
    constraints(Constraints).
constraint(C) -->
    term(C).

to_clpq(A > B,   C) => C = (A #>  B).
to_clpq(A < B,   C) => C = (A #<  B).
to_clpq(A >= B,  C) => C = (A #>= B).
to_clpq(A =< B,  C) => C = (A #=< B).
to_clpq(A = B,   C) => C = (A #=  B).
to_clpq(A =\= B, C) => C = (A #<> B).
to_clpq(X, Y)       => Y = X.

term(T) -->
    term(T,
         [ quoted(true),
           numbervars(true)
         ]).
