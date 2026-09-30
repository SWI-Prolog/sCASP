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

:- module(sscasp_runtime, []).
:- reexport(solve,
            [ check/7,
              proved/4,
              done/4,
              absent/3,
              dcc/4,
              for_domain/6,
              forall/5,
              findall_/6,
              not_findall_/6,
              compare_values/1,
              domain_check/3,
              sld_goal/5
            ]).
:- reexport(herbrand,
            [ apart/2,
              not_in_list/2,
              ('∈')/2
            ]).
:- reexport(library(clpq),
            [ {}/1
            ]).

/** <module> What a compiled program calls

The clauses `emit.pl` writes call the solver's runtime **unqualified** —
`check(...)`, not `sscasp_solve:check(...)` — and `run.pl:load_program/2`
makes that work by adding this module to the import modules of the module the
program is loaded into.  So this is the whole of the interface between a
compiled program and the solver: a predicate the program calls is exported
here, and one it does not call is not.

Three things follow from having it in one place:

  - **What the emitter may write is written down.**  A goal `emit.pl`
    starts writing into a clause body that is not exported here is an
    existence error in the first program that reaches it, rather than a
    name that happens to resolve because some module was imported whole.
  - **The cross referencer sees the calls.**  Nothing in any source file
    calls `dcc/4` or `not_in_list/2`; they are called by asserted clauses.
    Re-exported here, they are used.
  - **Which `{}/1` is meant is said once.**  `library(clpr)` exports a
    `{}/1` too, over a different domain; the emitted comparisons mean the
    rational one.

Every head the program asserts is mangled (`scasp$plain$flies$1`), so
nothing exported here can be shadowed by a program predicate.
*/
