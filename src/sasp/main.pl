/*
* Copyright (c) 2016, University of Texas at Dallas
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the University of Texas at Dallas nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY OF TEXAS AT DALLAS BE LIABLE FOR
* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

:- module(sasp_main,
          [ sasp_load/1     % +Sources:list
          ]).

/** <module> s(ASP) Ungrounded Stable Models Solver

Read in a normal logic program. Compute dual rules and the NMR check. Execute
the modified program according to the stable model semantics and output the
results.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

:- use_module(common).
:- use_module(comp_duals).
:- use_module(io).
:- use_module(nmr_check).
:- use_module(options).
:- use_module(program). % for destroy_program/0
:- use_module(debug).
:- use_module(options).
:- use_module(output).

%!  sasp_load(+Sources:list)
%
%   Load the files from Sources.   Steps taken:
%
%     - Parse input and assert in dynamic predicates with
%       program.pl (defined_rule/3, etc,)
%     - Enrich the program in the same format (comp_duals/0,
%       generate_nmr_check/0).
%     - Transform into _pr_ rules (generate_pr_rules/1)
%     - Destroy the program dynamic predicates.
%
%   @arg Sources A list of paths of input files.

sasp_load(Sources) :-
    setup_call_cleanup(
        set_default_options,
        sasp_load_guarded(Sources),
        ( destroy_program,
          option_cleanup)).

sasp_load_guarded(Sources) :-
    load_source_files(Sources),
    comp_duals,
    generate_nmr_check,
    write_verbose(0, 'Preparation of input program complete.\n'),
    generate_pr_rules(Sources),
    if_debug(0, write_program).
