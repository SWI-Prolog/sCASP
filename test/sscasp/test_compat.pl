/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_compat,
          [ test_compat/0
          ]).
:- use_module(library(sscasp/compat)).
:- use_module(library(plunit)).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(filesex), [directory_file_path/3, delete_directory_and_contents/1]).
:- use_module(library(process), [process_create/3, process_wait/2]).
:- use_module(library(lists), [append/3, member/2]).

/** <module> library(sscasp/compat): a program written for library(scasp)

The compatibility layer only rewrites files from outside this checkout,
so each program here is written to a temporary directory and loaded from
there.  Loading this suite installs the rewriting for the rest of the
process, which is why `test_all.pl` loads it last.
*/

test_compat :-
    run_tests([ compat, switch ]).

%!  old_program(+Name, +Text, -Module) is det.
%
%   Write Text as module Name in a fresh temporary directory and load it.

old_program(Name, Text, Name) :-
    tmp_file(compat, Dir),
    make_directory(Dir),
    file_name_extension(Name, pl, Base),
    directory_file_path(Dir, Base, File),
    setup_call_cleanup(open(File, write, Out),
                       ( format(Out, ":- module(~q, []).~n", [Name]),
                         format(Out, Text, [])
                       ),
                       close(Out)),
    load_files(File, [silent(true)]),
    delete_directory_and_contents(Dir).

:- begin_tests(compat).

%   A bare import gives library(sscasp)'s scasp/2, and the old HTML
%   renderer and the assert family from compat/.

test(a_bare_import) :-
    old_program(compat_bare,
                ":- use_module(library(scasp)).~n\c
                 q(a). q(b). r(b).~n\c
                 p(X) :- q(X), not r(X).~n", M),
    assertion(predicate_property(M:scasp(_,_), imported_from(sscasp_dyncall))),
    assertion(predicate_property(M:scasp_assert(_), imported_from(scasp_compat))),
    assertion(\+ predicate_property(M:html_model(_,_,_,_), imported_from(_))),
    findall(X, M:scasp(p(X), []), Xs),
    assertion(Xs == [a]).

%   An import list takes scasp/2 from compat/, which returns the
%   justification in the old shape.

test(an_import_list) :-
    old_program(compat_list,
                ":- use_module(library(scasp), [scasp/2]).~n\c
                 q(a).~n\c
                 p(X) :- q(X).~n", M),
    assertion(predicate_property(M:scasp(_,_), imported_from(scasp_compat))),
    once(M:scasp(p(X), [tree(Tree)])),
    assertion(X == a),
    assertion(Tree = query-_),
    assertion(sub_term(o_nmr_check, Tree)).

%   The old library interned -s(X) as '-s'(X); a clause written that way
%   is a clause of -/1.

test(the_interned_negation) :-
    old_program(compat_neg,
                ":- use_module(library(scasp)).~n\c
                 q(a).~n\c
                 '-s'(X) :- q(X).~n", M),
    assertion(clause(M:(-(s(_))), _)),
    findall(Y, M:scasp(-s(Y), []), Ys),
    assertion(Ys == [a]).

%   library(scasp/Sub) is compat/Sub, and `:: ` and `pred` are operators
%   before the program is read.

test(a_sub_library_and_the_operators) :-
    old_program(compat_sub,
                ":- use_module(library(scasp)).~n\c
                 :- use_module(library(scasp/output)).~n\c
                 :- use_module(library(scasp/html)).~n\c
                 :- describe p(_X) :: \"@(X) holds\".~n\c
                 p(a).~n", M),
    assertion(predicate_property(M:ovar_analyze_term(_), imported_from(scasp_compat_output))),
    assertion(predicate_property(M:html_model_term(_,_,_,_), imported_from(scasp_compat_html))),
    assertion(current_op(950, xfx, user:(::))).

:- end_tests(compat).


%!  switched(+Text, -Status, -Output) is det.
%
%   Write Text to a file and load it in `swipl -Dsscasp`, with check/0
%   as the goal.  `--on-error=status` makes an error printed while
%   loading count, which is how an import conflict shows.

switched(Text, Status, Output) :-
    in_tmp_dir(program, Text, Dir, File),
    run_prolog(Dir, ['-Dsscasp', '--on-error=status',
                     '-g', check, '-t', halt, File], Status, Output),
    delete_directory_and_contents(Dir).

%!  in_tmp_dir(+Base, +Text, -Dir, -File) is det.
%
%   Write Text to Base.pl in a new temporary directory.

in_tmp_dir(Base, Text, Dir, File) :-
    tmp_file(switch, Dir),
    make_directory(Dir),
    file_name_extension(Base, pl, Name),
    directory_file_path(Dir, Name, File),
    setup_call_cleanup(open(File, write, Out),
                       format(Out, Text, []),
                       close(Out)).

%!  run_prolog(+Cwd, +Args, -Status, -Output) is det.
%
%   Run a copy of this Prolog in Cwd: from this checkout, with no packs,
%   so that library(scasp) is this tree's, with this pack's `app`
%   directory, and with no init file, which might expand the import
%   itself.  Cwd is not the checkout, whose built `scasp` would be taken
%   for a script by `swipl scasp`.

run_prolog(Cwd, Args, Status, Output) :-
    checkout_dir(Root),
    directory_file_path(Root, prolog, Lib),
    directory_file_path(Root, app, App),
    atom_concat('library=', Lib, LibArg),
    atom_concat('app=', App, AppArg),
    append(['-f', none, '--no-packs', '-p', LibArg, '-p', AppArg],
           Args, AllArgs),
    process_create(prolog(self), AllArgs,
                   [ stdin(null), stdout(pipe(PO)), stderr(pipe(PO)),
                     process(PID), cwd(Cwd)
                   ]),
    read_string(PO, _, Output),
    close(PO),
    process_wait(PID, exit(Status)).

checkout_dir(Root) :-
    source_file(checkout_dir(_), Here),
    file_directory_name(Here, Sscasp),
    file_directory_name(Sscasp, Test),
    file_directory_name(Test, Root).

:- begin_tests(switch).

%   Every sub-library library(sscasp/compat) provides, imported next to
%   library(scasp) into one module.  html_model//2 is one predicate by
%   either route, scasp_query_clauses/2 is the old one and comes only
%   from scasp/dyncall, and nothing of the old solver is loaded.

switch_program(Order, Text) :-
    Imports0 = [ ":- use_module(library(scasp)).~n" ],
    Subs = [ ":- use_module(library(scasp/dyncall)).~n",
             ":- use_module(library(scasp/html)).~n",
             ":- use_module(library(scasp/human)).~n",
             ":- use_module(library(scasp/listing)).~n",
             ":- use_module(library(scasp/model)).~n",
             ":- use_module(library(scasp/options)).~n",
             ":- use_module(library(scasp/output)).~n"
           ],
    (   Order == scasp_first
    ->  append(Imports0, Subs, Imports)
    ;   append(Subs, Imports0, Imports)
    ),
    atomic_list_concat(Imports, Header),
    atomic_list_concat(
        [ Header,
          "q(a). q(b). r(b).~n",
          "p(X) :- q(X), not r(X).~n",
          "from(PI, M) :- PI = N/A, functor(H, N, A),~n",
          "    predicate_property(user:H, imported_from(M)).~n",
          "check :-~n",
          "    from(scasp/2, sscasp_dyncall),~n",
          "    from(html_model/4, scasp_compat_html),~n",
          "    from(html_model_term/4, scasp_compat_html),~n",
          "    from(scasp_query_clauses/2, scasp_compat_dyncall),~n",
          "    from(ovar_analyze_term/1, scasp_compat_output),~n",
          "    from(print_model/2, scasp_compat_model),~n",
          "    \\+ current_module(scasp_solve),~n",
          "    findall(X, scasp(p(X), []), [a]).~n"
        ], Text).

test(sub_libraries_after_library_scasp, Status == 0) :-
    switch_program(scasp_first, Text),
    switched(Text, Status, _).
test(sub_libraries_before_library_scasp, Status == 0) :-
    switch_program(scasp_last, Text),
    switched(Text, Status, _).

%   library(scasp/json) takes the old results dict and writes the new
%   document: the tree is a forest, the constraints a list per variable.

test(json, Status == 0) :-
    switched(":- use_module(library(scasp)).~n\c
              :- use_module(library(scasp/json)).~n\c
              q(1).~n\c
              p(X) :- q(Y), X #> Y.~n\c
              check :-~n\c
              once(scasp(p(X), [model(M), tree(T)])),~n\c
              scasp_results_json(_{query:p(X), cpu:0.1,~n\c
                                   answers:[_{answer:1, bindings:['X'=X],~n\c
                                              model:M, tree:user:T}]}, D),~n\c
              D.time == 0.1,~n\c
              D.answers = [A],~n\c
              is_list(A.tree),~n\c
              A.constraints = _{'A':[_{type:(>), value:_}]}.~n",
             Status, _).

%   `swipl scasp`, the pack's app, is the old executable, and the new one
%   under the flag.  Told apart by the exit status for a program with no
%   answer, which the new one says is 1.

test(app, Statuses == [0, 1, 1]) :-
    in_tmp_dir(none, "q(1).~n?- q(2).~n", Dir, File),
    findall(Status,
            ( member(Args, [ [scasp], ['-Dsscasp', scasp], [sscasp] ]),
              append(Args, [File], AllArgs),
              run_prolog(Dir, AllArgs, Status, _)
            ),
            Statuses),
    delete_directory_and_contents(Dir).

:- end_tests(switch).
