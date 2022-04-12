:- module(scasp_messages,
          []).
:- use_module(lang/en, []).
:- use_module(library(solution_sequences)).

:- create_prolog_flag(scasp_lang, default, [keep(true)]).

:- multifile
    scasp_lang/2.

:- dynamic lang_module_cache/1 as volatile.

%!  lang_module(-M) is multi.
%
%   True when M is a module holding rules for scasp_message//1.

lang_module(M) :-
    (   lang_module_cache(M)
    *-> true
    ;   forall(distinct(gen_lang_module(M)),
               assertz(lang_module_cache(M))),
        lang_module_cache(M)
    ).

gen_lang_module(Module) :-
    msg_language(Lang),
    clean_encoding(Lang, Clean),
    longest_id(Clean, LangID),
    scasp_lang(LangID, Module).
gen_lang_module(Module) :-
    scasp_lang(en, Module).

clean_encoding(Lang0, Lang) :-
    (   sub_atom(Lang0, A, _, _, '.')
    ->  sub_atom(Lang0, 0, A, _, Lang)
    ;   Lang = Lang0
    ).

%!  msg_language(-Lang) is det.
%
%   Get the current language for messages.

msg_language(Lang),
    current_prolog_flag(scasp_lang, Lang0),
    Lang0 \== default =>
    Lang = Lang0.
msg_language(Lang),
    \+ current_prolog_flag(windows, true),
    setlocale(messages, Lang0, Lang0) =>
    Lang = Lang0.
msg_language(Lang),
    getenv('LANG', Lang0) =>
    Lang = Lang0.
msg_language(Lang) =>
    Lang = en.

longest_id(Lang, Id) :-
    split_string(Lang, "_-", "", Components),
    longest_prefix(Components, Taken),
    atomic_list_concat(Taken, '_', Id).

longest_prefix(List, List).
longest_prefix([_|T], Prefix) :-
    longest_prefix(T, Prefix).

:- multifile
    prolog:message//1.

prolog:message(scasp(Term)) -->
    [ 's(CASP): '-[] ],
    { lang_module(Module)
    },
    Module:scasp_message(Term).

