:- module(scasp_options,
          [ if_user_option/2,
            set/2,
            parse_args/3,
            current_option/2,
            set_options/1,
            s_help/0
          ]).

/** <module> (Command line) option handling for sCASP

@author Joaquin Arias
*/

:- meta_predicate
    if_user_option(+, 0).

:- dynamic current_option/2.

%!  scasp_version
%
%   print the current version of s(CASP)

scasp_version :-
    format('s(CASP) version ~p\n',['swi.0.21.08.03']),
    halt.

%!  set_options(+Options) is det.
%
%   Set the user options. Options is  a   list  of atoms that denote the
%   commandline arguments that start with `-`.

set_options(Options) :-
    set_default_options,
    set_user_options(Options),
    set_default_tree_options,
    check_compatibilities.

set_default_options :-
    retractall(current_option(_, _)),
    set(answers,-1),
    set(verbose,0),
    set(unicode,true).

set_default_tree_options :-
    (   current_option(print_tree,on)
    ->  (   \+ current_option(short,on),
            \+ current_option(long,on)
        ->  set(mid,on)
        ;   true
        ),
        (   \+ current_option(pos,on)
        ->  set(neg,on)
        ;   true
        )
    ;   true
    ).

check_compatibilities :-
    current_option(check_calls,on),
    current_option(human,on), !,
    format('ERROR: verboser and human output do not allowed together!\n\n',[]),
    s_help,
    halt(1).
check_compatibilities.


set_user_options([]).
set_user_options([O|Os]) :-
    (   set_user_option(O)
    ->  set_user_options(Os)
    ;   format('ERROR: The option ~w is not supported!\n\n',[O]),
        s_help,
        halt(1)
    ).

set_user_option('--help_all')           :- help_all, halt.
set_user_option('-h')                   :- s_help, halt.
set_user_option('-?')                   :- s_help, halt.
set_user_option('--help')               :- s_help, halt.
set_user_option('-i')                   :- set(interactive, on).
set_user_option('--interactive')        :- set(interactive, on).
set_user_option('-a').
set_user_option('--auto').
set_user_option(Option)                 :- atom_chars(Option,['-','s'|Ns]),
                                           number_chars(N,Ns),
                                           set(answers,N).
set_user_option(Option)                 :- atom_chars(Option,['-','n'|Ns]),
                                           number_chars(N,Ns),
                                           set(answers,N).
set_user_option('-c')                   :- set(compiled, on).
set_user_option('--compiled')           :- set(compiled, on).

set_user_option('-d')                   :- set(plain_dual, on).
set_user_option('--plaindual')          :- set(plain_dual, on).

set_user_option('-r')                   :- set(real, on), set(decimals,5).
set_user_option(Option)                 :- atom_concat('-r=',Ns,Option),
                                           atom_number(Ns,D),set(real,on),
                                           set(decimals,D).

set_user_option('--code')               :- set(write_program, on), set(neg,on).
set_user_option('--tree')               :- set(process_stack, on),
                                           set(print_tree, on).
set_user_option('--tree*')              :- set(process_stack, on),
                                           set(print_tree, on),
                                           set(assume,on).

set_user_option('--plain')              .
set_user_option('--human')              :- set(human, on).

set_user_option('--long')               :- set(long,on).
set_user_option('--mid')                :- set(mid,on).
set_user_option('--short')              :- set(mid,on), set(short,on).

set_user_option('--neg')                :- set(neg,on).
set_user_option('--pos')                :- set(pos,on).

set_user_option('-u')                   :- set(unicode, true).
set_user_option('--unicode')            :- set(unicode, true).

set_user_option('--html')               :- set(process_stack, on), set(html, on).
set_user_option(Option)                 :- atom_concat('--html=',File,Option),
                                           set(html_name, File),
                                           set(process_stack, on),
                                           set(html, on).

set_user_option('-v')                   :- set(check_calls, on).
set_user_option('--verbose')            :- set(check_calls, on).
set_user_option('-f0')                  :- set(trace_failures, on).
set_user_option('-f')                   :- set(trace_failures, on),
                                           set(show_tree,on).
set_user_option('--tracefails')         :- set(trace_failures, on),
                                           set(show_tree,on).
set_user_option('--version')            :- scasp_version.
% Development
set_user_option('-no')                  :- set(no_nmr, on).         %% skip the evaluation of nmr-checks (but compile them).
set_user_option('--no_nmr')             :- set(no_compile_nmr, on),
					   set(no_compile_olon, on).
set_user_option('--no_olon')            :- set(no_compile_olon, on).
set_user_option('-w')                   :- set(warning, on).
set_user_option('--warning')            :- set(warning, on).
set_user_option('--variant')            :- set(no_fail_loop, on).
set_user_option(Option)                 :- undef_option(Option, Undef),
					   set(undefined, Undef).
%% Only with tabling
set_user_option('-m')                   :- set(minimal_model,on).
set_user_option('--minimal')            :- set(minimal_model,on).
set_user_option('--all_c_forall')       :- set(all_forall,on).
set_user_option('--prev_forall')        :- set(prev_forall,on).
set_user_option('--raw')                :- set(raw,on).

undef_option(Option, Val) :-
    atom_concat('--undef=', Val, Option),
    must_be(oneof([silent,warning,error]), Val).


%!  if_user_option(:Name, :Call)
%
%   If the flag Name is `on` then the call Call is executed

if_user_option(Name, Call) :-
    (   current_option(Name,on)
    ->  call(Call)
    ;   true
    ).

%!  set(+Option, +Value)
%
%   Used to set-up the user options

set(Option, Value) :-
    retractall(current_option(Option, _)),
    assert(current_option(Option,Value)).

s_help :-
    format('Usage: scasp [options] InputFile(s)\n\n'),
    format('s(CASP) computes stable models of predicate normal logic programs with contraints\n'),
    format('  using a top-down evaluation algorihtm.\n'),
    format('Command-line switches are case-sensitive!\n\n'),
    format('General Options:\n\n'),
    format('  -h, -?, --help        Print this help message and terminate.\n'),
    format('  --help_all            Print extended help.\n'),
    format('  -i, --interactive     Run in interactive mode (REP loop).\n'),
    format('  -a, --auto            Run in batch mode (no user interaction).\n'),
    format('  -sN, -nN              Compute N answer sets, where N >= 0. N = 0 means ''all''.\n'),
    format('  -c, --compiled        Load compiled files (e.g. extracted using --code).\n'),
    format('  -d, --plaindual       Generate dual program with single-goal clauses\n'),
    format('                        (for propositional programs).\n'),
    format('  -r[=d]                Output rational numbers as real numbers.\n'),
    format('                        [d] determines precision. Defaults to d = 5.\n'),
    format('\n'),
    format('  --code                Print program with dual clauses and exit.\n'),
    format('  --tree                Print justification tree for each answer (if any).\n'),
    format('\n'),
    format('  --plain               Output code / justification tree as literals (default).\n'),
    format('  --human               Output code / justification tree in natural language.\n'),
    format('\n'),
    format('  --long                Output long version of justification.\n'),
    format('  --mid                 Output mid-sized version of justification (default) .\n'),
    format('  --short               Short version of justification.\n'),
    format('\n'),
    format('  --pos                 Only format the selected literals in the justification.\n'),
    format('  --neg                 Add the negated literals in the justification (default).\n'),
    format('\n'),
    format('  --html[=name]         Generate HTML file for the justification. [name]:\n'),
    format('                        use \'name.html\'. Default: first InputFile name.\n'),
    format('\n'),
    format('  -v, --verbose         Enable verbose progress messages.\n'),
    format('  -f, --tracefails      Trace user-predicate failures.\n'),
    format('  --version             Output the current version of s(CASP)\n'),
    format('\n'),
    format('  --all_c_forall        Exhaustive evaluation of c_forall/2.\n'),
    format('  --prev_forall         Deprecated evaluation of forall/2.\n'),
    format('\n').

help_all :-
    help,
    format('  --no_olon             Do not compile olon rules (for debugging purposes).\n'),
    format('  --no_nmr              Do not compile NMR checks (for debugging purposes).\n'),
    format('  -w, --warning         Enable warning messages (failures in variant loops / disequality).\n'),
    format('  --undef=Mode          Check for undefined predicates (silent,warning,error)\n'),
    format('  --variant             Do not fail in the presence of variant loops.\n'),
    format('\n'),
    format('  -m, --minimal         Collect only the minimal models (TABLING required).\n'),
    format('  --raw                 Sort the clauses as s(ASP) does (use with --code).\n'),
    format('\n').


%!  parse_args(?Args, ?Options, ?Sources)
%
%   Select  from  the  list  of   arguments  in   Args  which   are  the
%   user-options, Options and which are the program files, Sources

parse_args([],[],[]).
parse_args([O|Args], [O|Os], Ss) :-
    atom_concat('-',_,O),!,
    parse_args(Args, Os, Ss).
parse_args([S|Args], Os, [S|Ss]) :-
    parse_args(Args, Os, Ss).
