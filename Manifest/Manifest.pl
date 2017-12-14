:- bundle(tclp_asp).

depends([
    core,
    lpdoc
]).

alias_paths([
    tclp_asp = 'src'
]).

lib('src').

manual('tclp_asp', [main='doc/SETTINGS.pl']).

