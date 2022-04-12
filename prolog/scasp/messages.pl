:- module(scasp_messages,
          [ scasp_lang/1,			 % -Lang
            scasp_justification_message//1       % +Term
          ]).
:- use_module(lang/en, []).
:- use_module(lang/nl, []).                      % Dynamic?
:- use_module(library(solution_sequences)).

:- create_prolog_flag(scasp_lang, default, [keep(true)]).

:- multifile
    scasp_lang_module/2.

:- thread_local lang_module_cache/1 as volatile.

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
    scasp_lang(Lang),
    clean_encoding(Lang, Clean),
    longest_id(Clean, LangID),
    scasp_lang_module(LangID, Module).
gen_lang_module(Module) :-
    scasp_lang_module(en, Module).

clean_encoding(Lang0, Lang) :-
    (   sub_atom(Lang0, A, _, _, '.')
    ->  sub_atom(Lang0, 0, A, _, Lang)
    ;   Lang = Lang0
    ).

%!  scasp_lang(-Lang) is det.
%
%   True when Lang is the language used for messages and justifications.

scasp_lang(Lang),
    current_prolog_flag(scasp_lang, Lang0),
    Lang0 \== default =>
    Lang = Lang0.
:- if(current_prolog_flag(windows, true)).
scasp_lang(Lang),
    win_get_user_preferred_ui_languages(name, [Lang0|_]) =>
    Lang = Lang0.
:- else.
scasp_lang(Lang),
    catch(setlocale(messages, _, ''), _, fail) =>
    setlocale(messages, Lang0, Lang0),
    Lang = Lang0.
:- endif.
scasp_lang(Lang),
    getenv('LANG', Lang0) =>
    Lang = Lang0.
scasp_lang(Lang) =>
    Lang = en.

longest_id(Lang, Id) :-
    split_string(Lang, "_-", "", Components),
    longest_prefix(Components, Taken),
    atomic_list_concat(Taken, '_', Id).

longest_prefix([H|T0], [H|T]) :-
    longest_prefix(T0, T).
longest_prefix(_, []).

:- multifile
    prolog:message//1.

prolog:message(scasp(Term)) -->
    [ 's(CASP): '-[] ],
    { lang_module(Module)
    },
    Module:scasp_message(Term).

scasp_justification_message(Term) -->
    { lang_module(Module)
    },
    Module:scasp_message(Term),
    !.
