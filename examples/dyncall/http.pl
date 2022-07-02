/*  Part of sCASP

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- use_module(library(scasp)).
:- use_module(library(scasp/html)).
:- use_module(library(scasp/output)).
:- use_module(library(scasp/json)).

:- use_module(library(http/http_server)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_error)).
:- use_module(library(http/jquery)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/term_html)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_host)).

% Become Unix daemon process when on Unix and not attached to a
% terminal.  Otherwise be a normal commandline application.

:- if((current_prolog_flag(unix,true),
       \+ stream_property(current_input, tty(true)))).

:- format(user_error, '% Loading as Unix daemon~n', []).
:- use_module(library(http/http_unix_daemon)).

:- else.

:- initialization(main,main).

main(Argv) :-
    argv_options(Argv, _, Options),
    (   select_option(examples(Ex), Options, Options1)
    ->  attach_examples(Ex)
    ;   Options1 = Options
    ),
    server(Options1),
    (   option(interactive(true), Options1)
    ->  cli_enable_development_system
    ;   thread_get_message(quit)  % do not terminate
    ).

opt_type(port,     port,        natural).
opt_type(p,        port,        natural).
opt_type(examples, examples,    atom).
opt_type(i,        interactive, boolean).

opt_help(port,        "Port used by the server").
opt_help(interactive, "Start interactive toplevel").
opt_help(examples,    "':' separated list of files and directories from \c
                       which to load examples").

opt_meta(port,     'PORT').
opt_meta(examples, 'DIRECTORY').

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

:- http_handler(root(.),        http_redirect(see_other, scasp(.)), []).
:- http_handler(scasp(.),       home,         []).
:- http_handler(scasp(solve),   solve,        [id(solve)]).
:- http_handler(scasp(help),    scasp_help,   [id(help)]).
:- http_handler(scasp(example), example_data, [id(example_data)]).

		 /*******************************
		 *        HTML FORM PAGE	*
		 *******************************/

%!  home(+Request)
%
%   HTML handler to generate the home page.   This  is a simple form the
%   can send requests and display the result.

home(_Request) :-
    http_link_to_id(help, [], Help),
    reply_html_page([ title('s(CASP) web server')
                    ],
                    [ h1(['s(CASP) web server ', a(href(Help), 'Help')]),
                      \query_page
                    ]).

query_page -->
    html_requires(jquery),
    html_requires(scasp),
    styles,
    html([ div(class(section),
               [ div(class(select), \example_data),
                 textarea([id(data), rows(10), cols(80),
                           placeholder('s(CASP) program')], '')
               ]),
           h4('Query'),
           '?- ',     input([id(query), size(50),
                             placeholder('Query')]),
           ' Limit ', input([type(number), min(1), id(limit), value(1),
                             size(3),
                             placeholder('Empty means all answer sets')]),
           \html_json_radio,
           h4(''),
           button(id(solve), 'Solve'),
           button(id(clear), 'Clear'),
           div(id('results-html'), []),
           pre(id('results-json'), [])
         ]),
    button_actions.

%!  example_data
%
%   Fill the example data

example_data -->
    { \+ example(_,_,_) },
    !.
example_data -->
    { findall(Id-Comment, example(Id,_File,Comment), Pairs),
      http_link_to_id(example_data, [], ExDataURL)
    },
    html([ select([name(example), id(example)],
                  \sequence(example_option, Pairs))
         ]),
    js_script({|javascript(ExDataURL)||
$(function() {
$("#example").change(function() {
  var ex = $(this).val();

  $.get(ExDataURL,
        {"example": ex},
        function(reply) {
            var rows = reply.data.split("\n").length + 1;
            if ( rows > 20 ) rows = 20;
            $("#data").val(reply.data)
                      .attr("rows", rows);
            $("#query").val(reply.query || "");
        });
});

});
               |}).

example_option(Id-Comment) -->
    html(option(value(Id), Comment)).

example_data(Request) :-
    http_parameters(Request, [example(Id, [atom])]),
    example(Id, File, Comment),
    read_file_to_string(File, Data, []),
    Reply0 = _{comment:Comment, data:Data},
    (   re_matchsub('^\\?-\\s*(.*?)\\.($|\\s)'/m, Data, Dict, [])
    ->  Query = Dict.1,
        Reply = Reply0.put(query, Query)
    ;   Reply = Reply0
    ),
    reply_json(Reply).

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
  $("#results-html").empty();
  $("#results-json").empty();
  $.get(SolveURL,
        { data: data,
          query: query,
          limit: limit,
          format: format
        },
        function(reply) {

          if ( typeof(reply) == "string" ) {
            var results = $("#results-html");
            results.html(reply);
            results.sCASP('swish_answer');
          } else if ( typeof(reply) == "object" ) {
            var results = $("#results-json");
            results.text(JSON.stringify(reply, undefined, 2));
          }
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


%!  solve(+Request)
%
%   Service is s(CASP) request. See the  help   page  of  the server for
%   details.

solve(Request) :-
    option(method(post), Request),
    option(content_type(Type), Request),
    sub_atom(Type, 0, _, _, 'text/x-scasp'),
    !,
    http_read_data(Request, Data, [to(string)]),
    solve(Request, #{ data:Data, format:json }).
solve(Request) :-
    option(method(post), Request),
    option(content_type(Type), Request),
    sub_atom(Type, 0, _, _, 'application/json'),
    !,
    http_read_json(Request, Dict0, [json_object(dict)]),
    foldl(to_atom, [format], Dict0, Dict1),
    foldl(dict_default, [format(json)], Dict1, Dict),
    solve(Request, Dict).
solve(Request) :-
    http_parameters(Request,
                    [ data(Data, []),
                      query(QueryS, [optional(true)]),
                      limit(Limit, [optional(true), integer]),
                      format(Format, [default(html)]),
                      tree(Tree, [boolean,optional(true)])
                    ]),
    solve(Request,
          #{ data:Data,
             query:QueryS,
             limit:Limit,
             format:Format,
             tree:Tree}).

to_atom(Key, Dict0, Dict) :-
    (   Value = Dict0.get(Key)
    ->  atom_string(Atom, Value),
        Dict = Dict0.put(Key, Atom)
    ;   Dict = Dict0
    ).

dict_default(Term, Dict0, Dict) :-
    Term =.. [Name,Value],
    (   _ = Dict0.get(Name)
    ->  Dict = Dict0
    ;   Dict = Dict0.put(Name, Value)
    ).

%!  solve(+Request, +Dict)
%
%   The actual solver.

solve(_Request, Dict) :-
    _{ data:Data,
       query:QueryS,
       limit:Limit,
       format:Format,
       tree:Tree } >:< Dict,

    ignore(Limit = infinite),
    solve_options(Format, Tree, Options),

    % prepare the program
    Error = error(Formal,_),
    catch(( setup_call_cleanup(
                open_string(Data, In),
                read_terms(In, Terms0),
                close(In)),
            query(QueryS, Query, Bindings, Terms0, Terms)
          ),
          Error,
          true),

    % On error, display the error.  Else solve the query.
    (   nonvar(Formal)
    ->  reply_html_page([],
                        \error(Error))
    ;   in_temporary_module(
            M,
            ( use_module(library(scasp)),
              maplist(add_to_program(M), Terms)
            ),
            ( call_time(findall(Result,
                                scasp_result(M:Query, Bindings, Result, Options),
                                Results),
                        TotalTime),
              ovar_set_bindings(Bindings),
              ovar_analyze_term(Query, []),
              inline_constraints(Query, []),
              reply(Format, M:Query, TotalTime, Results)
            ))
    ).

error(Error) -->
    { message_to_string(Error, Message) },
    html(div(class(error), Message)).

solve_options(html, _, Options) =>
    Options = [tree(true), unknown(fail)].
solve_options(json, Tree, Options) =>
    ignore(Tree=true),
    Options = [tree(Tree), unknown(fail), inline_constraints(false)].

%!  read_terms(+In:stream, -Terms) is det.
%
%   Read the program terms and (optional) query from In.

read_terms(In, Terms) :-
    read_one_term(In, Term0, Pos0),
    read_terms(Term0, Pos0, In, Terms).

read_terms(end_of_file, _, _, []) :-
    !.
read_terms(Term0, Pos0, In, [Term0-Pos0|T]) :-
    read_one_term(In, Term, Pos),
    read_terms(Term, Pos, In, T).

read_one_term(In, Term, Pos) :-
    read_term(In, Term,
              [ module(scasp_dyncall),
                variable_names(Bindings),
                term_position(Pos)
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
fixup_pred((?- Query), Bindings) =>
    fixup_atom(Query, Bindings).
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

%!  add_to_program(+Module, +Term)
%
%   Add clauses to the program.  Also handles s(CASP) directives.

add_to_program(M, (# Directive)-Pos) =>
    #(M:Directive, Pos).
add_to_program(M, (:- show Spec)-_) =>
    show(M:Spec).
add_to_program(M, (:- pred Spec)-_) =>
    pred(M:Spec).
add_to_program(M, (:- abducible Spec)-Pos) =>
    abducible(M:Spec, Pos).
add_to_program(M, Term-Pos) =>
    scasp_assert(M:Term, Pos).

%!  query(+String, -Query, -Bindings, +ProgramIn, -ProgramOut) is det.

query(QueryS, Query, Bindings, Terms, Terms) :-
    atomic(QueryS), QueryS \== '',
    !,
    term_string(Query, QueryS, [variable_names(Bindings)]).
query(_, Query, Bindings, TermsIn, TermsOut) :-
    partition(is_query, TermsIn, Queries, TermsOut),
    last(Queries, (?- Query0)-_),
    !,
    varnumbers_names(Query0, Query, Bindings).
query(_, _, _, _, _) :-
    existence_error(scasp_query, program).

is_query((?-_)-_) => true.
is_query(_) => false.

%!  scasp_result(+Query, +Bindings, -Dict, +Options) is nondet.
%
%   True when we succeeded proving Query.  Dict contains details such as
%   the model and justification tree.

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
    call_nth(call_time(scasp(Query,
                             [ model(M:Model),
                               tree(M:Tree)
                             | Options
                             ]),
                       Time), Counter),
    analyze_variables(t(Bindings, Model, Tree), Bindings, Options).
scasp_result(Query,
             Bindings,
             scasp{ query:Query,
                    answer:Counter,
                    bindings:Bindings,
                    model:Model,
                    time:Time
                  },
             Options) :-
    Query = M:_,
    call_nth(call_time(scasp(Query,
                             [ model(M:Model)
                             | Options
                             ]), Time),
             Counter),
    analyze_variables(t(Bindings, Model), Bindings, Options).

analyze_variables(Term, Bindings, Options) :-
    ovar_set_bindings(Bindings),
    (   option(inline_constraints(false), Options)
    ->  ovar_analyze_term(Term, [name_constraints(true)])
    ;   ovar_analyze_term(Term, []),
        inline_constraints(Term, [])
    ).

%!  reply(+Format, :Query, +TotalTime, +Results)
%
%   Reply in either `html` or `json` format.

reply(html, M:_Query, TotalTime, Results) =>
    reply_html_page([],
                    \results(Results, M, TotalTime)).
reply(json, Query, TotalTime, Results) =>
    scasp_results_json(#{query:Query,
                         answers:Results,
                         cpu:TotalTime},
                       JSON),
    reply_json_dict(JSON, []).


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
               \justification_section(M:Result)
             ])).

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

justification_section(M:Results) -->
    { Tree = Results.get(tree) },
    !,
    html_justification_tree(M:Tree, []).
justification_section(_) -->
    [].


		 /*******************************
		 *             HELP		*
		 *******************************/

scasp_help(Request) :-
    reply_html_page([ title('s(CASP) web server -- help')
                    ],
                    [ h1(['The s(CASP) web server']),
                      \help_page(Request)
                    ]).

help_page(Request) -->
    { http_public_host_url(Request, Home),
      http_link_to_id(solve, [], URL),
      atom_concat(Home, URL, Solve)
    },
    html({|html(Solve)||
<h2>Diclaimer</h2>

<blockquote>
This is a <b>demonstrator</b>.  This service is likely to change functionality and location.
This service runs on <a href="https://www.swi-prolog.org">SWI-Prolog</a> using the
<a href="https://github.com/JanWielemaker/sCASP">[GitHub] SWI-Prolog port of s(CASP)</a>.  Bugs
should be reported at the issue tracker of the the GIT repository.  Discussion can use the
<a href="https://swi-prolog.discourse.group/">SWI-Prolog forum</a>

</blockquote>

<h2>About</h2>

<p>The <code>s(CASP)</code> system is a top-down interpreter for ASP programs with
constraints.</p>

<p>This work was presented at ICLP'18 (<a href="https://www.cambridge.org/core/journals/theory-and-practice-of-logic-programming/article/constraint-answer-set-programming-without-grounding/55A678C618EF54487777F021D89B3FE7">Arias et al. 2018</a>), also available <a href="https://arxiv.org/abs/1804.11162">here</a>.</p>

<p>And extended description of the justification trees was presented at ICLP'20 (<a href="http://www.cliplab.org/papers/sCASP-ICLP2020/TC-explainCASP.pdf">Arias et al. 2020</a>).</p>

<p><code>s(CASP)</code> by <a href="mailto:joaquin.arias@urjc.es">Joaquin Arias</a>, is based on
<a href="https://sourceforge.net/projects/sasp-system/"><code>s(ASP)</code></a> by
Kyle Marple.</p>

<p><code>s(CASP)</code> is an implementation of the stable model semantics of
constraint logic programming. Unlike similar systems, it does not
employ any form of grounding. This allows <code>s(CASP)</code> to execute programs
that are not finitely groundable, including those which make use of
lists and terms.</p>

<h2>Using as a service</h2>

<p>
The server accepts data requests on <span>Solve</span>.  It deals with several
formats:
</p>

  <ul>
    <li>POST using <code>Content-type: text/x-scasp</code><br>
        In this case the document is plain text containing both the
        program and the query as <code>?- Query</code>.  Using <code>curl</code>
        we can use this script:

        <pre>
        curl --data-binary @file.pl -H "Content-Type: text/x-scasp" -X POST <code>Solve</code>
        </pre>
    </li><li>POST using <code>Content-type: application/json</code><br>
        In this case the request is a JSON document containing these keys:
        <ul>
          <li> <code>data</code><br>
               This key is required and contains the program.
          </li><li><code>query</code><br>
               This key is required if the program does not contain a query.  If
               the program contains a query, the specified query in the JSON
               request is used, <b>not</b> the one in the program.
          </li><li><code>limit</code><br>
               Max number of answers to return.  Default is all.
          </li><li><code>format</code><br>
               One of <code>html</code> or <code>json</code>.  Default is <code>json</code>.
          </li><li><code>tree</code><br>
               Boolean that indicates whether the justification is produced.  Default
               <code>true</code>.
          </li>
        </ul>
    </li><li>POST using <code>Content-type: application/x-www-form-urlencoded</code><br>
        As above.  The default reply format is <code>html</code>.
    </li><li>GET<br>
        Interpret the query parameters as above.  The default reply format is <code>html</code>.
    </li>
  </ul>
         |}).


		 /*******************************
		 *           EXAMPLES		*
		 *******************************/

:- dynamic
    example/3.

attach_examples(Spec) :-
    atomic_list_concat(Ex, ':', Spec),
    maplist(attach_example, Ex).

attach_example(Dir) :-
    exists_directory(Dir),
    !,
    forall(directory_member(Dir, File,
                            [ recursive(true),
                              extensions([pl]),
                              access(read)
                            ]),
           attach_ex(File)).
attach_example(File) :-
    exists_file(File),
    !,
    attach_ex(File).
attach_example(File) :-
    print_message(warning, error(existence_error(file, File),_)).

attach_ex(File) :-
    variant_sha1(File, Id),
    ex_comment(File, Comment),
    assertz(example(Id, File, Comment)).


ex_comment(File, Comment) :-
    setup_call_cleanup(
        open(File, read, In),
        ( read_line_to_string(In, Line),
          sub_atom(Line, 0, _, _, '%'),
          sub_atom(Line, 1, _, 0, Comment0),
          split_string(Comment0, "", " \t%", [Title])
        ),
        close(In)),
    !,
    file_base_name(File, Base),
    format(string(Comment), '~w -- ~w', [Base, Title]).
ex_comment(File, Comment) :-
    file_base_name(File, Comment).
