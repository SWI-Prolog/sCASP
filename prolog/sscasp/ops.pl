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
them in its own export list.

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
particular `:- domain day = mon ; tue.` is **not** a `domain/1` term: `;` is
1100 and binds looser, so the term is `;(domain(day=mon), tue)` and nothing
would notice.  Hence the list syntax -- see `domains.pl`.
*/
