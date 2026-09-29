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

:- module(sscasp_lang,
          [ connector/2                 % @Meaning, -Text
          ]).
:- use_module(ops).

/** <module> The words a justification is made of

Almost everything `html.pl` says that is not the program's own vocabulary is
in this table — the literals and connectors, though not yet the few fixed
phrases around a query and a typed placeholder that `html.pl` writes itself: `not` is "there is no evidence that", `implies` is "because", `#>` is
"is greater than".  One place, so that the renderer names a *meaning* and
never a word, and a second language is a second table rather than a reading
of every clause in `html.pl`.

The old implementation keeps these in `lang/en.pl` and `lang/nl.pl` mixed in
with the solver's and the CLI's diagnostics, selected by the `scasp_lang`
flag.  They are separated here because they are not the same kind of thing:
a diagnostic is for whoever runs the program and this is part of the
*answer*.  Only English is written; the flag is not read yet, and adding a
language means adding a table and the two lines that choose between them.
*/

%!  connector(@Meaning, -Text) is semidet.
%
%   Text is how this language says Meaning.  Fails for a meaning it has
%   no words for, which is how `html.pl` asks "is this a comparison I
%   should read out?" — so a new operator falls back on being printed
%   rather than on being mis-said.
%
%   @arg Meaning is an atom naming a connective, or a comparison such as
%       `_ #> _`, whose arguments are ignored.

connector(and,       'and').
connector(or,        'or').
connector(not,       'there is no evidence that').
connector(may,       'it may be the case that').
connector(-,         'it is not the case that').
connector(implies,   'because').
connector(?,         '?').
connector(proved,    'justified above').
connector(chs,       'it is assumed that').
connector(holds,     ' holds').
connector(holds_for, ' holds for ').
connector(not_in,    'is not').
connector(neq,       'is not').
connector(global_constraints_hold,
                     'The global constraints hold').

connector(_ >  _,  'is greater than').
connector(_ >= _,  'is greater than or equal to').
connector(_ <  _,  'is less than').
connector(_ =< _,  'is less than or equal to').
connector(_ #>  _, 'is greater than').
connector(_ #>= _, 'is greater than or equal to').
connector(_ #<  _, 'is less than').
connector(_ #=< _, 'is less than or equal to').
connector(_ #=  _, 'is equal to').
connector(_ #<> _, 'is not equal to').
connector(_ =  _,  'is').
connector(_ \= _,  'is not').
connector(_ is _,  'is').
