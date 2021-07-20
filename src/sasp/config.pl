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

:- module(config,
          [ default_option/2,
            stack_size/1
          ]).

/** <module> Default options for compiler.

Facts for default options. These can be overridden by command-line switches.
To change the default behavior of the compiler, change the default_option/2
facts below. Additionally, the stack size is defined here, and cannot be changed
via command-line switches.

@author Kyle Marple
@version 20170127
@license BSD-3
*/

%!  default_option(+Option:atom, +Value:compound) is det
%
%   Declarations      for      default      options.        Used      by
%   main:set_default_options/0. Only one fact should  be declared for an
%   Option value.
%
%   @arg Option An identifier associated with the option.
%   @arg Value The default value for the option.

% Execution mode: auto (automatic) or user (interactive).
default_option(mode, auto).

% Number of answer sets to compute in automatic mode. 0 for all.
default_option(ascount, 1).

% Debugging level. Levels >= 0 are not intended for use by end-users. Change at
% your own risk.
default_option(debug, -1).

% Justification. If true, the proof tree will be printed after each solution.
default_option(justification, false).
default_option(html_justification, false).
default_option(statistics_run_time, false).

% Abducibles. If true, print a separate list of abducibles with each CHS. If no
% abducibles have succeeded, output will be unaffected.
default_option(list_abducibles, false).

% Hide goals added by global consistency check. If true, these goals will not be
% printed. This affects output ONLY; the goals will still be executed normally.
default_option(hide_nmr, false).

%!  stack_size(+Size:atom)
%
%   Override for default SWI stack sizes.   Increasing this value allows
%   for compilation of larger programs, which  may otherwise yield stack
%   errors. All three stacks are set to   this  value: global, local and
%   trail.
%
%   This will only work with 64-bit SWI.  This option will be ignored on
%   32-bit systems, as the limit is already   set to the maximum, 128MB.
%   Additionally, if the limit is set   higher than the system supports,
%   it will be silently reduced to the maximum allowed size.
%
%   @arg Size The maximum stack size to use. Values is an integer number of
%        kilobytes. For large values, use powers of 10, ex. X*10**9 for X
%        gigabytes.

stack_size(2*10**9).
