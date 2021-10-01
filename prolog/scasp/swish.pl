:- module(scasp_swish,
          []).
:- use_module(library(http/html_write)).
:- use_module(library(pengines)).
:- use_module(swish(lib/config)).

:- use_module(embed).
:- use_module(html).

/** <module> s(CASP) adapter for SWISH
*/

:- multifile
    swish_config:config/2,
    swish_trace:post_context/3.

swish_config:config(scasp_model_var, '_scasp_model').
swish_config:config(scasp_justification_var, '_scasp_justification').

swish_trace:post_context(Name, _Goal,  _) :-
    swish_config(residuals_var, Name),
    scasp_model(_),
    !.
swish_trace:post_context(Name, _Goal, HTMLModel) :-
    swish_config:config(scasp_model_var, Name),
    pengine_self(Module),
    scasp_model(Module:Model),
    html_string(html_model(Model, []), HTMLModel).
swish_trace:post_context(Name, _Goal, HTMLJustification) :-
    swish_config:config(scasp_justification_var, Name),
    pengine_self(Module),
    scasp_justification(Module:Tree, []),
    html_string(html_justification_tree(Tree, []), HTMLJustification).

:- meta_predicate
    html_string(//, -).

html_string(Goal, HTML) :-
    phrase(Goal, Tokens),
    with_output_to(string(HTML0), print_html(Tokens)),
    split_string(HTML0, "", "\n ", [HTML]).
