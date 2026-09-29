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

:- module(sscasp_ops,
          [ op(900,  fy,  not),
            op(700,  xfx, [#=, #<>, #<, #>, #=<, #>=]),
            op(700,  xfx, '∉'),            % ∉
            op(700,  xfx, '∈'),            % ∈
            op(700,  xfx, '| '),           % Var | {Constraints}
            op(1150, fx,  domain),
            op(1150, fx,  sld),
            op(1150, fx,  describe),
            op(950,  xfx, ::),
            op(1150, fx,  pred),
            op(200,  xfx, ..)
          ]).

/** <module> Operators

Every operator the language needs, in one place, so that a module wanting them
says `:- use_module(ops)` and a module re-exporting them to *its* callers lists
them in its own export list, at the same priorities.  Two readers add their
own on top: `source.pl` the directives of an s(CASP) file (`#`, `include`,
`abducible`, `show`, ...), which only a file has, and `prolog/sscasp.pl` the
`?`-shortcuts and the directives a Prolog file may write.  Where the two
declare the same name — `abducible`, `show` — it is at the same priority, so
`#abducible p` means one thing whichever reads it.

| `not` | default negation |
| `#=`, `#<>`, `#<`, `#>`, `#=<`, `#>=` | CLP(Q) comparison |
| `∉`, `∈` | the values a variable may not, or may, take |
| `| ` | an answer's variable beside what constrains it -- `render.pl` |
| `domain`, `pred` | the annotation directives |
| `sld` | `:- sld p/2.` — "prove this one by SLD resolution" |
| `describe`, `::` | `:- describe flies(X) :: "@(X) can fly".` |
| `..` | an integer range in a domain declaration |

`..` is ours.  SWI does not define it, and it is what lets
`:- domain hour = 0..23.` parse without parentheses.

`sld` is deliberately not called `prolog`: a prefix operator of that name
makes `prolog:message//1` a syntax error in every file that can see it, and
the old implementation has exactly that wart.  Naming the *resolution* rather
than the language avoids it and says more — it leaves room for a goal proved
some other way to have its own word.

`domain`, `pred` and `sld` are prefix operators at 1150, the priority directives use,
which is worth knowing because it decides how their arguments parse.  In
particular `:- domain day = mon ; tue.` is `domain((day = mon ; tue))`: `;`
is 1100 and binds tighter, so the disjunction is the domain's body, and
`domains.pl` refuses it with a domain error.  Hence the list syntax.
*/
