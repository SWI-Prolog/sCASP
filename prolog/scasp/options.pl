:- module(scasp_options,
          [ scasp_parse_args/3,            % +Argv, -Sources, -Options
            scasp_help/0,
            scasp_set_options/1,           % +Options
            scasp_set_options/2,           % +Options, -Unprocessed
            scasp_version/1,               % -Version
            scasp_opt_type/3,              % ?Flag, ?Option, ?Type
            scasp_opt_help/2,              % +Option, -Help
            scasp_opt_meta/2               % +Option, -Meta
          ]).
:- use_module(library(main)).
:- use_module(library(strings)).           % Quasi quotation
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).

/** <module> (Command line) option handling for sCASP

@author Joaquin Arias
*/

% Prefer Unicode symbols over ASCII
:- create_prolog_flag(scasp_unicode, true, []).

%!  scasp_version(-Version)
%
%   print the current version of s(CASP)

scasp_version('1.1.4').

%!  scasp_set_options(+Options) is det.
%!  scasp_set_options(+Options, -Unprocessed) is det.
%
%   Set Prolog flags that control the solver from Options.

scasp_set_options(Options) :-
    scasp_set_options(Options, _).

scasp_set_options(Options, Left) :-
    opt_process(Options, Options1),
    exclude(set_option, Options1, Left).

% Solver options
set_option(nmr(Bool)) =>
    set_prolog_flag(scasp_compile_nmr, Bool).
set_option(olon(Bool)) =>
    set_prolog_flag(scasp_compile_olon, Bool).
set_option(forall(Algorithm)) =>
    set_prolog_flag(scasp_forall, Algorithm).
set_option(dcc(Bool)) =>
    set_prolog_flag(scasp_dcc, Bool).
% Presentation uptions
set_option(unicode(Bool)) =>
    set_prolog_flag(scasp_unicode, Bool).
set_option(assume(Bool)) =>
    set_prolog_flag(scasp_assume, Bool).
set_option(real(Decimals)) =>
    set_prolog_flag(scasp_real, Decimals).
% Verbosity options
set_option(verbose(Bool)) =>
    set_prolog_flag(scasp_verbose, Bool).
set_option(warning(Bool)) =>
    set_prolog_flag(scasp_warnings, Bool).
set_option(trace_fails(Bool)) =>
    set_prolog_flag(scasp_trace_failures, Bool).
set_option(trace_dcc(Bool)) =>
    set_prolog_flag(scasp_trace_dcc, Bool).
set_option(raw(Bool)) =>
    set_prolog_flag(scasp_list_raw, Bool).
set_option(color(Bool)) =>
    set_prolog_flag(color_term, Bool).
% Ignore other well formed options.
set_option(Term), compound(Term), functor(Term, _, 1) =>
    fail.

		 /*******************************
		 *        OPTION CHECKING	*
		 *******************************/

%!  opt_process(+OptionsIn, -Options) is det.
%
%   Post processs the option list. This   does a findall/3 on opt_rule/1
%   which may use opt/1 to access the   option list being processed. The
%   opt_rule/1 returns one or more actions.  Defined actions are:
%
%     - default(+Opt)
%       If Opt is not defined, add Opt as default.
%     - add(+Opt)
%       Add an option.  If the option is defined, remove it.
%     - replace(+Opts, +Opt)
%       Remove options for Opts (a list or a single option) and
%       add Opt.
%     - warning(Term)
%       call print_message(warning, Term) and continue.
%     - error(Term)
%       call print_message(error, Term) and die using halt(1).

%   @tbd This is code that might go into the Prolog libraries at some time.

:- det(opt_process/2).
opt_process(Options0, Options) :-
    opt_step(Options0, Options1),
    (   Options1 == Options0
    ->  Options = Options1
    ;   opt_step(Options1, Options)
    ).

opt_step(Options0, Options) :-
    findall(Act, opt_act(Options0, Act), Actions),
    foldl(apply_action, Actions, Options0, Options).

apply_action(default(Opt), Options0, Options) =>
    merge_options(Options0, [Opt], Options).
apply_action(add(Opt), Options0, Options) =>
    merge_options([Opt], Options0, Options).
apply_action(replace(Old, New), Options0, Options) =>
    opt_delete(Old, Options0, Options1),
    merge_options([New], Options1, Options).
apply_action(warning(Msg), Options0, Options) =>
    Options = Options0,
    print_message(warning, Msg).
apply_action(error(Msg), _Options0, _Options) =>
    print_message(error, Msg),
    halt(1).

opt_delete([], Options0, Options) =>
    Options = Options0.
opt_delete([H|T], Options0, Options), is_opt(H) =>
    delete(Options0, H, Options1),
    opt_delete(T, Options1, Options).
opt_delete(Opt, Options0, Options), is_opt(Opt) =>
    delete(Options0, Opt, Options).

is_opt(Opt) :-
    compound(Opt),
    compound_name_arity(Opt, _, 1).

opt_act(Options, Act) :-
    b_setval(options, Options),
    opt_rule(Act).

opt(Opt) :-
    b_getval(options, Opts),
    option(Opt, Opts).

%!  opt_rule(-Action) is nondet.
%
%   Option rules for s(CASP).  Processed using opt_process/2 above.

opt_rule(Action) :-
    detail(print_tree, Action).
opt_rule(Action) :-
    detail(write_program, Action).
opt_rule(Error) :-
    at_most_one_of([verbose, human], Error).
opt_rule(Error) :-
    at_most_one_of([interactive, human], Error).
opt_rule(add(forall(prev))) :-
    \+ opt(forall(_)),
    opt_true(dcc).
opt_rule(error(scasp(opt_dcc_prev_forall))) :-
    opt(forall(Forall)),
    Forall \== prev,
    opt_true(dcc).

detail(Opt, Action) :-
    True =.. [Opt,true],
    opt(True),
    (opt(short(Short)) -> true ; Short = '-'),
    (opt(mid(Mid))     -> true ; Mid   = '-'),
    (opt(long(Long))   -> true ; Long  = '-'),
    (   detail(Short, Mid, Long, Detail)
    ->  Del =.. [Opt,_],
        New =.. [Opt,Detail],
        Action = replace([Del,short(_),mid(_),long(_)],
                         New)
    ;   Action = error(scasp(at_most_one_of([short,mid,long])))
    ).

detail(true, -, -, short).
detail(-, true, -, mid).
detail(-, -, true, long).
detail(-, -, -,    mid).

at_most_one_of(List, Error) :-
    append(_, [First|Rest], List),
    opt_true(First),
    member(Second, Rest),
    opt_true(Second),
    !,
    Error = error(scasp(at_most_one_of(List))).

opt_true(Name) :-
    Opt =.. [Name,true],
    opt(Opt).


		 /*******************************
		 *             SPEC		*
		 *******************************/

%!  opt_type(?Opt, ?Destination, ?Type)

opt_type(interactive, interactive,    boolean).
opt_type(i,           interactive,    boolean).

opt_type(s,           answers,        nonneg).
opt_type(n,           answers,        nonneg).

opt_type(query,       query,          term([variable_names(_)])).

opt_type(compiled,    compiled,       boolean).
opt_type(c,           compiled,       boolean).

opt_type(plaindual,   plain_dual,     boolean).
opt_type(d,           plain_dual,     boolean).

opt_type(r,           real,           between(1,16)|oneof([float])).

opt_type(code,        write_program,  boolean).
opt_type(human,       human,          boolean).
opt_type(tree,        tree,           boolean).
opt_type(pos,         pos,            boolean).
opt_type(assume,      assume,         boolean).
opt_type(short,       short,          boolean).
opt_type(mid,         pos,            boolean).
opt_type(long,        long,           boolean).

opt_type(html,        html,           file(write)).
opt_type(css,         style,          boolean).
opt_type(script,      script,         boolean).
opt_type(collapse,    collapse_below, nonneg).

opt_type(json,        json,           file(write)).
opt_type(width,       width,          nonneg).

opt_type(unicode,     unicode,        boolean).
opt_type(u,           unicode,        boolean).

opt_type(color,       color,          boolean).

opt_type(verbose,     verbose,        boolean).
opt_type(v,           verbose,        boolean).

opt_type(version,     version,        boolean).

opt_type(forall,      forall,         oneof([all,all_c,prev,sasp])).

opt_type(trace_fails, trace_fails,    boolean).
opt_type(f,           trace_fails,    boolean).

opt_type(trace_dcc,   trace_dcc,      boolean).

opt_type(nmr,         nmr,            boolean(false)).
opt_type(olon,        olon,           boolean(false)).
opt_type(dcc,         dcc,            boolean).

opt_type(unknown,     unknown,        oneof([fail,warning,error])).

opt_type(warning,     warning,        boolean).
opt_type(w,           warning,        boolean).

opt_type(minimal,     minimal,        boolean).
opt_type(m,           minimal,        boolean).

opt_type(raw,         raw,            boolean).

opt_help(interactive,    "Run in interactive mode (REP loop)").
opt_help(answers,        "Number of answers to report (0 for all)").
opt_help(query,          "Query to run (overrules ?- query from program)").
opt_help(compiled,       "Load compiled files (e.g. extracted using --code)").
opt_help(write_program,  "Output the compiled program and exit").
opt_help(plain_dual,     "Generate dual program with single-goal clauses").
opt_help(real,           "Output rational numbers as decimal. \c
                          An integer value specifies the number of decimals \c
                          the value `float` simply converts to a float.").
opt_help(code,           "Print program with dual clauses and exit").
opt_help(human,          "Output code/justification tree in natural language").
opt_help(tree,           "Print justification tree for each answer").
opt_help(assume,         "Mark assumed nodes in the justification").
opt_help(long,           "Output long version of code or justification.").
opt_help(mid,            "Output mid-sized version of code or justification \c
			  (default)").
opt_help(short,          "Short version of code or justification").
opt_help(pos,            "Only format the selected literals in the \c
                          justification").
opt_help(html,           "Generate an HTML file (\"-\" for standard output)").
opt_help(style,          "Include CSS in HTML output (default)").
opt_help(script,         "Include JavaScript in HTML output (default)").
opt_help(collapse_below, "Collapse HTML tree below this level (2)").
opt_help(json,           "Generate a JSON file (\"-\" for standard output)").
opt_help(width,          "Page width.  For JSON, 0 stops formatting the output").

opt_help(unicode,        "Use Unicode symbols in output").
opt_help(color,          "Use ANSI sequences to color terminal output").
opt_help(verbose,        "Enable verbose progress messages").
opt_help(trace_fails,    "Trace user-predicate failures").
opt_help(trace_dcc,      "Trace DCC pruning").
opt_help(unknown,        "Act on undefined predicates (fail,warning,error)").
opt_help(forall,         "Forall algorithm to use ([all], all_c, prev, sasp)").
opt_help(olon,           "Compile olon rules (--no-olon for debugging purposes)").
opt_help(nmr,            "Compile NMR rules (--no-nmr for debugging purposes)").
opt_help(dcc,            "Use Dynamic Consistency Checking").
opt_help(warning,        "Enable warning messages (failures in variant \c
                          loops / disequality)").
opt_help(version,        "Print version and exit").
opt_help(variant,        "Do not fail in the presence of variant loops").
opt_help(minimal,        "Collect only the minimal models (TABLING required)").
opt_help(raw,            "Sort the clauses as s(ASP) does (use with --code)").

opt_help(help(header), [ansi(bold, '~w', [Header])]) :-
    scasp_version(Version),
    Header = {|string(Version)||
              | s(CASP) version {Version}
             |}.
opt_help(help(usage), Usage) :-
    Usage = {|string||
             | [options] file ...
             |
             | s(CASP) computes stable models of predicate normal logic programs
             | with contraints using a top-down evaluation algorihtm.
            |}.

opt_meta(answers,        'COUNT').
opt_meta(real,           'DECIMALS').
opt_meta(unknown,        'MODE').
opt_meta(collapse_below, 'LEVELS').
opt_meta(forall,	 'ALGORITHM').
opt_meta(width,		 'WIDTH').

%!  scasp_opt_type(?Flag, ?Option, ?Type).
%!  scasp_opt_help(?Option, ?Help).
%!  scasp_opt_meta(?Option, ?Meta).
%
%   Allow reusing scasp option processing

scasp_opt_type(Flag, Option, Type) :-
    opt_type(Flag, Option, Type).

scasp_opt_help(Option, Help) :-
    opt_help(Option, Help),
    Option \= help(_).

scasp_opt_meta(Option, Meta) :-
    opt_meta(Option, Meta).

%!  scasp_help
%
%   Print command line option help.

scasp_help :-
    argv_usage(debug).

%!  scasp_parse_args(+Args, -Sources, -Options)
%
%   Select  from  the  list  of   arguments  in   Args  which   are  the
%   user-options, Options and which are the program files, Sources.
%
%   This predicate calls halt/0 when called with ``--version``.

scasp_parse_args(Argv, Sources, Options) :-
    argv_options(Argv, Sources, Options0),
    opt_process(Options0, Options),
    info_and_exit_option(Sources, Options).

info_and_exit_option(_Sources, Options) :-
    info_option(Options),
    !,
    halt(0).
info_and_exit_option(_, _).

info_option(Options) :-
    option(version(true), Options),
    scasp_version(Version),
    current_prolog_flag(version, Num),
    Major is Num//10_000,
    Minor is (Num//100) mod 100,
    Patch is Num mod 100,
    atomic_list_concat([Major,Minor,Patch], '.', PlVersion),
    print_message(informational, scasp(version(Version, PlVersion))).
