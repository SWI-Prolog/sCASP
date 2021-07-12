:- module(scasp_ops,
          [ op(700, xfx, user:[.=., .\=.,.<>.,.<.,.=<.,.>.,.>=.]),
            op(700, xfx, user:[#= , #<>, #< , #> , #=<, #>= ]),
            op(700, xfx, user:[::]),
            op(700, xfx, user:['| ']),
            op(700, xfx, user:[~>, <~]),
            op(900, fy,  user:[not]),
            op(700, xfx, user:['\u2209'])
          ]).
