/*  `swipl scasp [option ...] file ...` runs s(CASP) as an app of this
    pack: the old implementation, or the new one under `swipl -Dsscasp`.

    Chosen here rather than left to prolog/scasp/main.pl, because that
    file loads from main.qlf when there is an up-to-date one, and the
    QLF holds the old implementation whatever the flag says.
*/

:- if(current_prolog_flag(sscasp, true)).
:- ensure_loaded('../prolog/sscasp/main').
:- else.
:- ensure_loaded('../prolog/scasp/main').
:- endif.
