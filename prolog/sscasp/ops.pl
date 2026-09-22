:- module(sscasp_ops,
          [ op(900,  fy,  not),
            op(700,  xfx, [#=, #<>, #<, #>, #=<, #>=]),
            op(700,  xfx, '∉'),            % ∉
            op(1150, fx,  domain),
            op(1150, fx,  pred),
            op(200,  xfx, ..)
          ]).

/** <module> Operators

Every operator the language needs, in one place, so that a module wanting them
says `:- use_module(ops)` and a module re-exporting them to *its* callers lists
them in its own export list.

| `not` | default negation |
| `#=`, `#<>`, `#<`, `#>`, `#=<`, `#>=` | CLP(Q) comparison |
| `∉` | constructive disunification against a list |
| `domain`, `pred` | the annotation directives |
| `..` | an integer range in a domain declaration |

`..` is ours.  SWI does not define it, and it is what lets
`:- domain hour = 0..23.` parse without parentheses.

`domain` and `pred` are prefix operators at 1150, the priority directives use,
which is worth knowing because it decides how their arguments parse.  In
particular `:- domain day = mon ; tue.` is **not** a `domain/1` term: `;` is
1100 and binds looser, so the term is `;(domain(day=mon), tue)` and nothing
would notice.  Hence the list syntax -- see `domains.pl`.
*/
