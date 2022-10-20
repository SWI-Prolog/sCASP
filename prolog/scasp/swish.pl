:- module(scasp_swish,
          []).
:- use_module(library(http/html_write)).
:- use_module(library(pengines)).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- use_module(swish(lib/config)).

:- use_module(library(scasp/embed)).
:- use_module(library(scasp/html)).
:- use_module(library(scasp/output)).

/** <module> s(CASP) adapter for SWISH

Hook into SWISH to make the  model   and  justification available in the
SWISH web interface.
*/

:- multifile
    swish_config:config/2,
    swish_trace:post_context/1,
    swish_trace:post_context/3.

%!  swish_trace:post_context(+Dict) is semidet.
%
%   Called before the other context extraction. We   use  it to name the
%   variables.    Note    that    we    also    do    the    work    for
%   swish_trace:post_context/3  here  because  we  need  to  remove  the
%   attributes.
%
%   The model and justification are  communicated   as  a  Prolog string
%   holding HTML. That is dubious as the SWISH infrastructure turns this
%   into escaped HTML which we need to undo in SWISH' `runner.js`.

swish_trace:post_context(Dict) :-
    _{bindings:Bindings0} :< Dict,
    swish_config:config(scasp_model_var, ModelVar),
    swish_config:config(scasp_justification_var, JustificationVar),
    selectchk(ModelVar = HTMLModel, Bindings0, Bindings1),
    selectchk(JustificationVar = HTMLJustification, Bindings1, Bindings),
    pengine_self(Module),
    scasp_model(Module:Model),
    scasp_justification(Module:Justification, []),
    Term = t(Bindings, Model, Justification),
    findall(HTMLModel-HTMLJustification, % revert backtrackable changes
            to_html(Module:Term, HTMLModel, HTMLJustification),
            [ HTMLModel-HTMLJustification ]).

:- det(to_html/3).

to_html(M:Term, HTMLModel, HTMLJustification) :-
    Term = t(Bindings, Model, Justification),
    maplist(set_name, Bindings),
    ovar_analyze_term(Term),
    inline_constraints(Term, []),
    html_string(html_model(M:Model, []), HTMLModel),
    html_string(html_justification_tree(M:Justification, []), HTMLJustification).

set_name(Name = Var) :-
    (   var(Var)
    ->  ovar_set_name(Var, Name)
    ;   true
    ).

swish_config:config(scasp_model_var, '_swish__scasp_model').
swish_config:config(scasp_justification_var, '_swish__scasp_justification').

%!  swish_trace:post_context(+Name, +Goal, -Var) is semidet.
%
%   Bind Var with the context  information   that  belongs to Name. Note
%   that we suppress normal  residuals  using   the  first  clause as we
%   report these through the others.  The   model  and justification are
%   already emitted in swish_trace:post_context/1 above.

swish_trace:post_context(Name, _Goal,  _) :-
    swish_config(residuals_var, Name),
    scasp_model(_),
    !.

:- meta_predicate
    html_string(//, -).

html_string(Goal, HTML) :-
    phrase(Goal, Tokens),
    with_output_to(string(HTML0), print_html(Tokens)),
    split_string(HTML0, "", "\n ", [HTML]).
