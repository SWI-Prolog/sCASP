:- module(scasp_options,
          [ parse_args/3,                  % +Argv, -Sources, -Options
            scasp_help/0,
            set_options/1                  % +Options
          ]).
:- use_module(library(main)).

/** <module> (Command line) option handling for sCASP

@author Joaquin Arias
*/

% Prefer Unicode symbols over ASCII
:- create_prolog_flag(scasp_unicode, true, []).

%!  scasp_version(-Version)
%
%   print the current version of s(CASP)

scasp_version('swi.0.21.08.03').

%!  set_options(+Options) is det.
%
%   Set Prolog flags that control the solver from Options.

set_options(Options) :-
    maplist(set_option, Options).

% Solver options
set_option(nmr(Bool)) =>
    set_prolog_flag(scasp_compile_nmr, Bool).
set_option(olon(Bool)) =>
    set_prolog_flag(scasp_compile_olon, Bool).
% Presentation uptions
set_option(unicode(Bool)) =>
    set_prolog_flag(scasp_unicode, Bool).
set_option(assume(Bool)) =>
    set_prolog_flag(scasp_assume, Bool).
% Verbosity options
set_option(verbose(Bool)) =>
    set_prolog_flag(scasp_verbose, Bool).
set_option(warning(Bool)) =>
    set_prolog_flag(scasp_warnings, Bool).
set_option(trace_fails(Bool)) =>
    set_prolog_flag(scasp_trace_failures, Bool).
set_option(raw(Bool)) =>
    set_prolog_flag(scasp_list_raw, Bool).
set_option(color(Bool)) =>
    set_prolog_flag(color_term, Bool).
% Ignore other well formed options.
set_option(Term), compound(Term), functor(Term, _, 1) =>
    true.

%!  default_options(+OptionsIn, -OptionsOut) is det.
%
%   Add additional default options

default_options(Options0, Options) :-
    default_tree_options(Options0, Options).

%!  default_tree_options(+OptionsIn, -OptionsOut) is det.
%
%   Fill in the default options about printing the tree.  This
%   results in the following options:
%
%     - tree(Detail)
%     - one of pos(true) or neg(true).

default_tree_options(Options0, Options) :-
    select_option(print_tree(true), Options0, Options1),
    !,
    tree_details(Options1, Options2),
    tree_pos(Options2, Options).
default_tree_options(Options, Options).

tree_details(Options0, [print_tree(Detail)|Options3]) :-
    select_option(short(Short), Options0, Options1, -),
    select_option(mid(Mid),     Options1, Options2, -),
    select_option(long(Long),   Options2, Options3, -),
    (   tree_detail(Short, Mid, Long, Detail)
    ->  true
    ;   at_most_one_of([short,mid,long])
    ).

tree_detail(true, -, -, short).
tree_detail(-, true, -, mid).
tree_detail(-, -, true, long).
tree_detail(-, -, -,    mid).

tree_pos(Options, Options) :-
    option(pos(true), Options),
    !,
    (   option(neg(true), Options)
    ->  at_most_one_of([pos,neg])
    ;   true
    ).
tree_pos(Options, Options) :-
    option(neg(true), Options),
    !.
tree_pos(Options, [neg(true)|Options]).


%!  check_options(+Options) is det.
%
%   Verify the consistency of the Options. Print a message and halt with
%   status 1 if there is a conflict.

check_options(Options) :-
    option(verbose(true), Options),
    option(human(true), Options),
    !,
    at_most_one_of([verbose,human]).
check_options(Options) :-
    option(interactive(true), Options),
    option(html(_), Options),
    !,
    at_most_one_of([interactive,html]).
check_options(_).

at_most_one_of(List) :-
    print_message(error, scasp(at_most_one_of(List))),
    halt(1).


		 /*******************************
		 *             SPEC		*
		 *******************************/

%!  opt_type(?Opt, ?Destination, ?Type)

opt_type(interactive, interactive,    boolean).
opt_type(i,           interactive,    boolean).

opt_type(s,           answers,        nonneg).
opt_type(n,           answers,        nonneg).

opt_type(compiled,    compiled,       boolean).
opt_type(c,           compiled,       boolean).

opt_type(plaindual,   plain_dual,     boolean).
opt_type(d,           plain_dual,     boolean).

opt_type(r,           real,           natural).

opt_type(code,        write_program,  boolean).
opt_type(human,       human,          boolean).
opt_type(tree,        tree,           boolean).
opt_type(assume,      assume,         boolean).

opt_type(html,        html,           file).
opt_type(css,         style,          boolean).
opt_type(script,      script,         boolean).
opt_type(collapse,    collapse_below, nonneg).

opt_type(unicode,     unicode,        boolean).
opt_type(u,           unicode,        boolean).

opt_type(color,       color,          boolean).

opt_type(verbose,     verbose,        boolean).
opt_type(v,           verbose,        boolean).

opt_type(version,     version,        boolean).

opt_type(forall,      forall,         oneof([all,all_c,prev])).

opt_type(tracefails,  trace_fails,    boolean).
opt_type(f,           trace_fails,    boolean).

opt_type(nmr,         nmr,            boolean(false)).
opt_type(olon,        olon,           boolean(false)).

opt_type(undef,       undefined,      oneof([silent,warning,error])).

opt_type(warning,     warning,        boolean).
opt_type(w,           warning,        boolean).

opt_type(minimal,     minimal,        boolean).
opt_type(m,           minimal,        boolean).

opt_type(raw,         raw,            boolean).

opt_help(interactive,    "Run in interactive mode (REP loop)").
opt_help(answers,        "Number of answers to report (0 for all)").
opt_help(compiled,       "Load compiled files (e.g. extracted using --code)").
opt_help(write_program,  "Output the compiled program and exit").
opt_help(plain_dual,     "Generate dual program with single-goal clauses").
opt_help(real,           "Output rational numbers as real numbers").
opt_help(code,           "Print program with dual clauses and exit").
opt_help(human,          "Output code/justification tree in natural language").
opt_help(tree,           "Print justification tree for each answer").
opt_help(assume,         "Mark assumed nodes in the tree").
opt_help(long,           "Output long version of justification.").
opt_help(mid,            "Output mid-sized version of justification (default)").
opt_help(short,          "Short version of justification").
opt_help(pos,            "Only format the selected literals in the \c
                          justification").
opt_help(neg,            "Add the negated literals in the justification \c
                          (default)").
opt_help(html,           "Generate an HTML file (\"-\" for standard output)").
opt_help(style,          "Include CSS in HTML output (default)").
opt_help(script,         "Include JavaScript in HTML output (default)").
opt_help(collapse_below, "Collapse HTML tree below this level (2)").
opt_help(unicode,        "Use Unicode symbols in output").
opt_help(color,          "Use ANSI sequences to color terminal output").
opt_help(verbose,        "Enable verbose progress messages").
opt_help(trace_fails,    "Trace user-predicate failures").
opt_help(undefined,      "Act on undefined predicates (silent,warning,error)").
opt_help(forall,         "Forall algorithm to use (all, [all_c], prev)").
opt_help(olon,           "Compile olon rules (--no-olon for debugging purposes)").
opt_help(nmr,            "Compile NMR rules (--no-nmr for debugging purposes)").
opt_help(warning,        "Enable warning messages (failures in variant \c
                          loops / disequality)").
opt_help(version,        "Print version and exit").
opt_help(variant,        "Do not fail in the presence of variant loops").
opt_help(minimal,        "Collect only the minimal models (TABLING required)").
opt_help(raw,            "Sort the clauses as s(ASP) does (use with --code)").

opt_help(usage,          "[options] file ...").

opt_meta(answers,        'COUNT').
opt_meta(real,           'DECIMALS').
opt_meta(undefined,      'MODE').
opt_meta(collapse_below, 'LEVELS').

%!  scasp_help
%
%   Print command line option help.

scasp_help :-
    argv_usage(debug).

%!  parse_args(+Args, -Sources, -Options)
%
%   Select  from  the  list  of   arguments  in   Args  which   are  the
%   user-options, Options and which are the program files, Sources

parse_args(Argv, Sources, Options) :-
    argv_options(Argv, Sources, Options0),
    info_and_exit_option(Sources, Options0),
    default_options(Options0, Options),
    check_options(Options).

info_and_exit_option(_Sources, Options) :-
    info_option(Options),
    !,
    halt(0).
info_and_exit_option(_, _).

info_option(Options) :-
    option(version(true), Options),
    scasp_version(Version),
    print_message(informational, scasp(version(Version))).
