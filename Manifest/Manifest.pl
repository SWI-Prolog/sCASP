
:- bundle(scasp).

depends([
    core,
    lpdoc
]).

alias_paths([
    scasp = 'src'
]).

lib('src').

manual('scasp', [main='doc/SETTINGS.pl']).

