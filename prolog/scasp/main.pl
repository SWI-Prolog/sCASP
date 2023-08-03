:- module(scasp_main,
          [ main/1,                         % +Argv

            op(700, xfx, [#= , #<>, #< , #> , #=<, #>= ]),
            op(900, fy, not),
            op(700, xfx, '\u2209')          % not element of
          ]).
:- set_prolog_flag(optimise, true).

:- use_module(compile).
:- use_module(ops).
:- use_module(options).
:- use_module(solve).
:- use_module(stack).
:- use_module(human).
:- use_module(output).
:- use_module(model).
:- use_module(listing).
:- use_module(html).
:- use_module(html_text).
:- use_module(json).
:- use_module(messages).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/json)).
:- use_module(library(dcg/high_order)).

:- meta_predicate
    print_query(:, +, +).

/** <module> sCASP as a stand-alone program

This module allows running scasp as a stand-alone program that loads one
or more scasp source files, answer the (last) query and exit.
*/

:- initialization(main, main).

%!  main(+Argv)
%
%   Used when calling from command  line   by  passing  the command line
%   options and the input files.

main(Argv) :-
    require_prolog_version('8.5.0-71',
                           [ warning(rational)
                           ]),
    catch_with_backtrace(
        ( scasp_parse_args(Argv, Sources, Options),
          scasp_set_options(Options),
          main(Sources, Options)
        ),
        Error,
        error(Error)).

main(Sources, Options) :-
    set_prolog_flag(rational_syntax, natural),
    set_prolog_flag(prefer_rationals, true),

    load_sources(Sources, Options),
    (   option(write_program(_), Options)
    ->  scasp_portray_program(Options),
        halt
    ;   option(interactive(true), Options)
    ->  '$toplevel':setup_readline,
        main_loop(Options)
    ;   query(Q, Bindings, Options)
    ->  ignore(main_solve(Q, [ variable_names(Bindings),
                               inputs(Sources)
                             | Options
                             ]))
    ).

load_sources([], _) :-
    !,
    print_message(error, scasp(no_input_files)),
    scasp_help,
    halt(1).
load_sources(Sources, Options) :-
    scasp_load(Sources, Options).

%!  error(Error)
%
%   Report expected errors concisely and  unexpected   ones  with a full
%   backtrace.

error(error(scasp_undefined(PIs), _)) :-
    !,
    maplist(report_undef, PIs),
    halt(1).
error(error(existence_error(source_sink, Source),_)) :-
    print_message(error, scasp(source_not_found(Source))),
    halt(1).
error(error(existence_error(scasp_query,scasp_main), _)) :-
    print_message(error, scasp(no_query)),
    halt(1).
error(PrologError) :-
    print_message(error, PrologError),
    halt(1).

report_undef(PI) :-
    print_message(error,
                  error(existence_error(scasp_predicate, PI), _)).


query(Query, Bindings, Options) :-
    option(query(Term-Bindings), Options),
    !,
    scasp_compile_query(Term, Query, Options).
query(Query, Bindings, Options) :-
    scasp_query(Query, Bindings, Options).

%!  main_loop(+Options)
%
%   Run an interactive toplevel loop.   Invoked  by `scasp -i ...`

main_loop(Options) :-
    read_term_with_history(R,
                           [ prompt('casp ~! ?- '),
                             variable_names(Bindings)
                           ]),
    (   atom(R),
        end_of_input(R)
    ->  format('~N'),
        halt
    ;   scasp_compile_query(R, Q, Options)
    ->  (   main_solve(Q, [variable_names(Bindings)|Options])
        ->  nl, main_loop(Options)
        ;   main_loop(Options)
        )
    ;   main_loop(Options)
    ).

end_of_input(end_of_file).
end_of_input(exit).
end_of_input(quit).
end_of_input(halt).

%!  main_solve(+Query, +Options) is semidet.
%
%   Solve a toplevel query. Query is a callable term where variables are
%   represented as $Name.
%
%   @tbd: If minimal_model(true) is given  we   must  select the minimal
%   model using printable_model/2 and simply print all answers.

main_solve(Query, Options) :-
    option(html(_File), Options),
    !,
    option(answers(MaxAnswers), Options, 0),
    option(variable_names(Bindings), Options, []),

    copy_term(Query-Bindings, QueryVar-QBindings),
    ovar_set_bindings(QBindings),
    inline_constraints(QueryVar, []),

    statistics(cputime, T0),
    (   MaxAnswers == 0
    ->  findall(Result,
                solve_results(Query, Bindings, Result, Options),
                Results)
    ;   findnsols(MaxAnswers, Result,
                  solve_results(Query, Bindings, Result, Options),
                  Results)
    ->  true
    ),
    statistics(cputime, T1),
    Time is T1-T0,

    html_print_results(scasp{ query:QueryVar,
                              cpu:Time,
                              answers:Results
                            },
                       Options).
main_solve(Query, Options) :-
    option(json(Name), Options),
    !,
    option(answers(MaxAnswers), Options, 0),
    option(variable_names(Bindings), Options, []),
    option(inputs(Inputs), Options, []),

    copy_term(Query-Bindings, QueryVar-QBindings),
    ovar_set_bindings(QBindings),
    inline_constraints(QueryVar, []),
    ROptions = [inline_constraints(false)|Options],

    statistics(cputime, T0),
    (   MaxAnswers == 0
    ->  findall(Result,
                solve_results(Query, Bindings, Result, ROptions),
                Results)
    ;   findnsols(MaxAnswers, Result,
                  solve_results(Query, Bindings, Result, ROptions),
                  Results)
    ->  true
    ),
    statistics(cputime, T1),
    Time is T1-T0,

    scasp_results_json(scasp{ query:QueryVar,
                              cpu:Time,
                              answers:Results,
                              inputs:Inputs
                            },
                       Dict),
    (   Name == '-'
    ->  json_write_dict(current_output, Dict, Options),
        format(current_output, '~N', [])
    ;   ensure_extension(Name, json, File),
        setup_call_cleanup(
            open(File, write, Out, [encoding(utf8)]),
            ( json_write_dict(Out, Dict, Options),
              format(current_output, '~N', [])
            ),
            close(Out))
    ).
main_solve(Query, Options) :-
    option(answers(MaxAnswers), Options, -1),
    option(variable_names(Bindings), Options, []),

    print_query(Query, Bindings, Options),
    statistics(cputime, T0),
    (   solve_results(Query, Bindings, Result, Options)
    *-> true
    ;   statistics(cputime, T1),
        T is T1-T0,
        print_message(warning, scasp(no_models(T))),
        fail
    ),

    print_answer(Result.answer, Result.time, Options),

    (   option(tree(_Detail), Options)
    ->  print_justification(Result.tree, Options)
    ;   true
    ),
    main_print_model(Result.model, Options),
    print_bindings(Bindings, [full_stop(false)|Options]),

    (   MaxAnswers == -1
    ->  allways_ask_for_more_models, nl, nl
    ;   MaxAnswers == 0
    ->  nl, nl,
        fail
    ;   MaxAnswers > 0
    ->  nl, nl,
        Result.answer = MaxAnswers
    ),
    !.

%!  solve_results(+Query, +Bindings, -Result:dict) is nondet.

solve_results(Query, Bindings,
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
    call_nth(call_time(solve(Query, [], StackOut, ModelOut), Time), Counter),
    justification_tree(StackOut, Tree, Options),
    canonical_model(ModelOut, Model),
    analyze_variables(t(Bindings, Model, StackOut), Bindings, Options).
solve_results(Query, Bindings,
              scasp{ query:Query,
                     answer:Counter,
                     bindings:Bindings,
                     model:Model,
                     time:Time
                   },
              Options) :-
    call_nth(call_time(solve(Query, [], _, ModelOut), Time), Counter),
    canonical_model(ModelOut, Model),
    analyze_variables(t(Bindings, Model), Bindings, Options).

analyze_variables(Term, Bindings, Options) :-
    ovar_set_bindings(Bindings),
    (   option(inline_constraints(false), Options)
    ->  ovar_analyze_term(Term, [name_constraints(true)])
    ;   ovar_analyze_term(Term, []),
        inline_constraints(Term, [])
    ).

:- if(\+current_predicate(tty_size/2)).
tty_size(25,80).
:- endif.

%!  print_answer(+Nth, +Resources:dict, +Options)

print_answer(Nth, Resources, Options) :-
    (   option(width(Width), Options)
    ->  true
    ;   catch(tty_size(_, Width), _, Width = 80)
    ),
    LineWidth is Width-8,
    ansi_format(comment, '~N% ~`\u2015t~*|~n', [LineWidth]),
    ansi_format(comment, '~N%', []),
    ansi_format(bold,    '~t Answer ~D (~3f sec) ~t~*|~n',
                [Nth, Resources.cpu, LineWidth]),
    ansi_format(comment, '~N% ~`\u2015t~*|~n', [LineWidth]).


%!  main_print_model(+Model, +Options)

main_print_model(Model, Options) :-
    ansi_format(comment, '~N% Model~n', []),
    print_model(Model,
                [ as_comment(false)
                | Options
                ]).

%!  print_query(:Query, +Bindings, +Options)

:- det(print_query/3).
print_query(M:Query, Bindings, Options) :-
    ansi_format(comment, '~N% Query~n', []),
    (   option(human(true), Options)
    ->  human_query(M:Query, [as_comment(false)|Options])
    ;   format('?- '),
        query_body(Query, PQ),
        portray_clause(current_output, PQ, [variable_names(Bindings)])
    ).

query_body(Query, Body) :-
    append(Q1, [o_nmr_check], Query),
    !,
    comma_list(Body, Q1).
query_body(Query, Body) :-
    comma_list(Body, Query).

%!  print_justification(+Tree, +Options)

print_justification(Tree, Options) :-
    ansi_format(comment, '~N% Justification~n', []),
    print_justification_tree(Tree, [ depth(1),
                                     as_comment(false)
                                   | Options
                                   ]).

print_bindings(Bindings, Options) :-
    exclude(empty_binding, Bindings, Bindings1),
    (   Bindings1 == []
    ->  ansi_format(comment, '~N% No bindings~n', []),
        (   option(full_stop(true), Options, true)
        ->  ansi_format(bold, 'true.', [])
        ;   ansi_format(bold, 'true', [])
        )
    ;   ansi_format(comment, '~N% Bindings~n', []),
        print_bindings1(Bindings1, Options)
    ).

empty_binding(Name = Value) :-
    Value == '$VAR'(Name).

print_bindings1([], _).
print_bindings1([H|T], Options) :-
    print_binding(H, Options),
    (   T == []
    ->  (   option(full_stop(true), Options, true)
        ->  format('.')
        ;   true
        )
    ;   format(',~n'),
        print_bindings1(T, Options)
    ).

print_binding(Name = Value, Options) :-
    format('~w = ~W',
           [ Name,
             Value, [ numbervars(true),
                      quoted(true),
                      portray(true)
                    | Options
                    ]
           ]).

% rational number handling

:- multifile
    user:portray/1.

user:portray(Rat) :-
    rational(Rat),
    current_prolog_flag(scasp_real, Decimals),
    (   integer(Decimals)
    ->  format('~*f', [Decimals, Rat])
    ;   Decimals == float
    ->  Value is float(Rat),
        write(Value)
    ).


%!  html_print_results(+Results:dict, +Options)
%
%   Options processed:
%
%     - human(Bool)
%       If `true`, open the HTML in human mode.  Default is to use
%       the formal notation.
%     - collapse_below(Depth)
%       Collapse the justification tree below the given level (default:
%       2).
%     - style(+Boolean)
%       When `false` (default `true`), do not include the HTML style
%       sheets.
%     - script(+Boolean)
%       When `false` (default `true`), do not include the JavaScript.

html_print_results(Results, Options) :-
    option(html(Name), Options),
    Name \== '-',               % stdout
    !,
    ensure_extension(Name, html, File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        ( phrase(reply(Results, Options), Tokens),
          print_html(Out, Tokens)
        ),
        close(Out)).
html_print_results(Results, Options) :-
    phrase(reply(Results, Options), Tokens),
    print_html(Tokens).

ensure_extension(Base, Ext, File) :-
    file_name_extension(_, Ext, Base),
    !,
    File = Base.
ensure_extension(Base, Ext, File) :-
    file_name_extension(Base, Ext, File).

reply(Results, Options) -->
    { length(Results.answers, Count)
    },
    emit_as(\page([],
                  [ \styles(Options),
                    \html_query(Results.query, Options),
                    \sequence(answer(Options), Results.answers),
                    div(class('scasp-statistics'),
                        [ h4('Statistics'),
                          p('~D answers in ~3f seconds'-[Count, Results.cpu])
                        ]),
                    \scripts(Options)
                  ]), html).

answer(Options, Answer) -->
    emit([ div(class('scasp-answer-header'),
               'Answer ~D (~3f seconds)'-[Answer.answer, Answer.time.cpu]),
           div(class('scasp-answer'),
               [ \html_bindings(Answer.bindings, Options),
                 \html_model(Answer.model, Options),
                 \html_justification_tree(Answer.tree, Options)
               ])
         ]).

%!  styles(+Options)
%
%   Include the style sheets unless ``--no-styles`` is given.

styles(Options) -->
    { option(style(false), Options) },
    !.
styles(_Options) -->
    { read_file(library('scasp/web/css/scasp.css'), SCASPCSS),
      read_file(library('http/web/css/plterm.css'), TermCSS)
    },
    html(style(SCASPCSS)),
    html(style(TermCSS)).

scripts(Options) -->
    { option(script(false), Options) },
    !.
scripts(Options) -->
    { (   option(human(true), Options)
      ->  As = human
      ;   As = machine
      ),
      option(collapse_below(Depth), Options, 2)
    },
    html(script(
             [ src('https://code.jquery.com/jquery-1.11.2.min.js'),
               integrity('sha256-Ls0pXSlb7AYs7evhd+VLnWsZ/AqEHcXBeMZUycz/CcA='),
               crossorigin('anonymous')
             ], [])),
    { read_file(library('scasp/web/js/scasp.js'), String) },
    html(script(String)),
    js_script({|javascript(As,Depth)||

$(function() {
  $("body").sCASP("swish_answer", {
    open_as:As,
    justification: {
      collapse_below: Depth,
      collapsed: false
    }
  });
});
              |}).

read_file(Spec, String) :-
    absolute_file_name(Spec, Path, [access(read)]),
    read_file_to_string(Path, String, []).

%!  ask_for_more_models is semidet.
%
%   Ask if the  user  want  to   generate  more  models  (execution from
%   console)".  __Fails__ if a next model is requested.

allways_ask_for_more_models :-
    (   format(' ? ', []),
        get_single_char(R),
        memberchk(R, `\s;`)
    ->  format(';\n'),
        fail
    ;   true
    ).
