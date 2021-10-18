:- module(scasp_main,
          [ main/1,                         % +Argv

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
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).

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
    parse_args(Argv, OptionsArgv, Sources),
    set_options(OptionsArgv),
    main_options(Options),
    main(Sources, Options).

main(Sources, Options) :-
    load_sources(Sources, Options),
    (   option(write_program(true), Options)
    ->  scasp_portray_program([duals(true)|Options]),
        halt
    ;   option(interactive(true), Options)
    ->  '$toplevel':setup_readline,
        main_loop(Options)
    ;   scasp_query(Q, Bindings, Options)
    ->  ignore(main_solve(Q, [variable_names(Bindings)|Options]))
    ).

load_sources([], _) :-
    !,
    print_message(error, scasp(no_input_files)),
    s_help,
    halt(1).
load_sources(Sources, Options) :-
    catch_with_backtrace(
        scasp_load(Sources, Options),
        Error,
        load_error(Error)).

load_error(error(scasp_undefined(PIs), _)) :-
    !,
    maplist(report_undef, PIs),
    halt(1).
load_error(PrologError) :-
    print_message(error, PrologError),
    halt(1).

report_undef(PI) :-
    print_message(error,
                  error(existence_error(scasp_predicate, PI), _)).


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
    ;   scasp_compile_query(R, Q, Options),
        (   main_solve(Q, [variable_names(Bindings)|Options])
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
    option(answers(Number), Options, -1),
    option(variable_names(Bindings), Options, []),
    (   option(html(true), Options)
    ->  copy_term(Query-Bindings, QueryVar-QBindings),
        ovar_set_bindings(QBindings),
        inline_constraints(QueryVar, [])
    ;   print_query(Query, Bindings, Options)
    ),

    (   call_nth(call_time(solve(Query, [], StackOut, ModelOut), Time), Counter)
    *-> true
    ;   ansi_format(warning, 'No models~n', []),
        fail
    ),

    print_answer(Counter, Time, Options),

    justification_tree(StackOut, Tree, Options),
    canonical_model(ModelOut, Model),

    All = t(Bindings, Model, StackOut),
    ovar_set_bindings(Bindings),
    ovar_analyze_term(All),
    inline_constraints(All, []),

    (   option(html(true), Options)
    ->  print_html(QueryVar, Bindings, Model, Tree, Options)
    ;   (   option(print_tree(true), Options)
        ->  print_justification(Tree, Options)
        ;   true
        ),
        main_print_model(Model, Options),
        print_bindings(Bindings, [full_stop(false)|Options])
    ),

    (   Number == -1
    ->  allways_ask_for_more_models, nl, nl
    ;   Number == 0
    ->  nl, nl,
        fail
    ;   Number > 0
    ->  nl, nl,
        Counter = Number
    ),
    !.

%!  main_options(-Options) is det.
%
%   Collect the relevant options from the options module.

main_options(Options) :-
    findall(Opt, main_option(Opt), Options).

main_option(html(true)) :-
    current_option(html, on).
main_option(html_name(File)) :-
    current_option(html_name, File).
main_option(human(true)) :-
    current_option(human, on).
main_option(human(true)) :-
    current_option(human, on).
main_option(answers(Number)) :-
    current_option(answers, Number).
main_option(print_tree(true)) :-
    current_option(print_tree, on).
main_option(nmr(false)) :-
    current_option(no_nmr, on).
main_option(undefined(Mode)) :-
    current_option(undefined, Mode).
main_option(write_program(true)) :-
    current_option(write_program, on).
main_option(interactive(true)) :-
    current_option(interactive, on).
main_option(constraints(true)).

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


%!  print_html(+Query, +Bindings, +Model, +Stack, +Options)
%
%   Options processed:
%
%     - open_as(As)
%       One of `machine` or `human` (default), indicating the initial
%       mode of the HTML elements.
%     - collapse_below(Depth)
%       Collapse the justification tree below the given level (default:
%       2).
%     - style(+Boolean)
%       When `false` (default `true`), do not include the HTML style
%       sheets.
%     - script(+Boolean)
%       When `false` (default `true`), do not include the JavaScript.

print_html(Query, Bindings, Model, Stack, Options) :-
    option(html_name(Name), Options),
    !,
    ensure_extension(Name, html, File),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        ( phrase(reply(Query, Bindings, Model, Stack, Options), Tokens),
          print_html(Out, Tokens)
        ),
        close(Out)).
print_html(Query, Bindings, Model, Stack, Options) :-
    phrase(reply(Query, Bindings, Model, Stack, Options), Tokens),
    print_html(Tokens).

ensure_extension(Base, Ext, File) :-
    file_name_extension(_, Ext, Base),
    !,
    File = Base.
ensure_extension(Base, Ext, File) :-
    file_name_extension(Base, Ext, File).

reply(Query, Bindings, Model, Tree, Options) -->
    emit_as(\page([],
                  [ \styles(Options),
                    h4('Query'),
                    \html_query(Query, Options),
                    h4('Bindings'),
                    \html_bindings(Bindings, Options),
                    \html_model(Model, Options),
                    \html_justification_tree(Tree, Options),
                    \scripts(Options)
                  ]), html).

styles(Options) -->
    { option(style(false), Options) },
    !.
styles(_Options) -->
    { read_file(library('scasp/web/css/scasp.css'), SCASPCSS),
      read_file(library('http/web/css/plterm.css'), TermCSS)
    },
    html(style(SCASPCSS)),
    html(style(TermCSS)).

styles(Options) -->
    { option(script(false), Options) },
    !.
scripts(Options) -->
    { option(open_as(As), Options, human),
      must_be(oneof([human,machine]), As),
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
