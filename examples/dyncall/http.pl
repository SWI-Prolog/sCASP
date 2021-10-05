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
%!  server(+Port)
%
%   Start HTTP server at the indicated port.

server(Port) :-
    http_server([port(Port)]).
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
    html([ h4('Program'),
           textarea([id(data), rows(10), cols(80),
                     placeholder('s(CASP) program')], ''),
           h4('Query'),
           '?- ',     input([id(query),
                             placeholder('Query')]),
           ' Limit ', input([type(number), min(1), id(limit), value(1),
                             placeholder('Empty means all answer sets')]),
           h4(''),
           button(id(solve), 'Solve'),
           button(id(clear), 'Clear'),
           div(id(results), [])
         ]),
    button_actions.

button_actions -->
    { http_link_to_id(solve, [], SolveURL) },
    js_script({|javascript(SolveURL)||
$(function() {

$("#solve").on("click", function() {
  var data = $("#data").val();
  var query = $("#query").val();
  var limit = $("#limit").val();
  $("#results").empty();
  $.get(SolveURL,
        { data: data,
          query: query,
          limit: limit
        },
        function(reply) {
          var results = $("#results");

          results.html(reply);
          results.find(".model")
                 .collapsable({collapsed:true,
                               delay:500
                              });
          results.find(".scasp-justification")
                 .collapsable({ collapsed:true,
                                tree:true,
                                buttons:true
                              });
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
                      limit(Limit, [optional(true), integer])
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
            ( call_time(findall(result(N, Time, M, VNames, Model, Justification),
                                call_nth(call_time(limit(Limit,
                                                         scasp(M:Query, Model, Justification)),
                                                   Time),
                                         N),
                                Results),
                        TotalTime),
              reply_html_page([],
                              \results(Results, TotalTime))
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

scasp(Query, Model, Justification) :-
    scasp(Query),
    scasp_model(Model),
    scasp_justification(Justification, []).

results([], Time) -->
    !,
    html(h3('No models (~3f sec)'-[Time.cpu])).
results(Results, _Time) -->
    sequence(result, Results).

result(result(N, Time, _M, Bindings, Model, Justification)) -->
    { maplist(set_name, Bindings),
      ovar_analyze_term(t(Bindings, Model, Justification))
    },
    html(div(class(result),
             [ h3('Result #~D (~3f sec)'-[N, Time.cpu]),
               \binding_section(Bindings),
               div(class([model, collapsable]),
                   [ h4(class('collapsable-header'), 'Model'),
                     \html_model(Model, [class('collapsable-content')])
                   ]),
               div(class(justification),
                   [ h4('Justification'),
                     \html_justification_tree(Justification, [])
                   ])
             ])).

set_name(Name = Var) :-
    (   var(Var)
    ->  ovar_set_name(Var, Name)
    ;   true
    ).

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
              [ module(scasp_dyncall)
              ]).

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
