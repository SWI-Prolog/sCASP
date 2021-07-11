% Load using
%
% swipl -l refactor.pl scasp.pl

:- use_module(library(refactor)).
:- use_module(library(dcg/basics)).
:- use_module(library(lynx/format)).

:- op(100,  fx, (?)).                   % pldoc
:- op(978, xfx, (::)).
:- op(500, yfx, (#)).
:- op(1150, fx,(pred)).
:- op(1150,xfx,(pred)).

pred(_).
doc(_,_).

replace_lpdoc :-
    forall(project_file(File),
           replace_lpdoc([file(File)])).

replace_lpdoc(Options) :-
    retractall(doc_data(_,_)),
    replace_sentence((:- pred Pred),
                      '$NODOT'('$TEXT'(PlDoc)),
                     lpdoc2pldoc(Pred, Dict, PlDoc),
                     [ variable_names(Dict)
                     | Options
                     ]),
    replace_sentence((:- doc(Type, Text)),
                     Replace,
                     replace_doc(Type, Text, Replace),
                     Options),
    replace_sentence((:- use_package(_)),
                     '$NODOT'('$TEXT'("")),
                     true,
                     Options),
    replace_sentence((:- use_module(library(CiaoLib))),
                     Replace,
                     ciao_lib(CiaoLib, Replace),
                     Options).

ciao_lib(CiaoLib, Replace) :-
    ciao_swi_lib(CiaoLib, SWILib),
    (   SWILib == []
    ->  Replace = '$NODOT'('$TEXT'(""))
    ;   Replace = (:- use_module(library(SWILib)))
    ).

ciao_swi_lib(terms_vars, []).


lpdoc2pldoc(Head : _ # Comment, Dict, PlDoc) =>
    lpdoc2pldoc(Head # Comment, Dict, PlDoc).
lpdoc2pldoc(Name/Arity # Comment, Dict, PlDoc) =>
    functor(Head, Name, Arity),
    numbervars(Head, 0, _),
    lpdoc2pldoc(Head # Comment, Dict, PlDoc).
lpdoc2pldoc(Head # Comment, Dict, PlDoc) =>
    maplist(bind_varname, Dict),
    lpdoc2markdown_percent(Comment, Dict, MarkDown,
                           [ width(68)
                           ]),
    format(string(PlDoc),
           '%!  ~W~n\c
            %~n\c
            %   ~w~n~n',
           [ Head, [numbervars(true), spacing(next_argument)],
             MarkDown
           ]).

bind_varname(Name=Var) :-
    Var = ?'$VAR'(Name).

lpdoc2markdown_percent(LpDoc, Dict, MarkDown, Options) :-
    lpdoc2markdown(LpDoc, Dict, MarkDown0, Options),
    split_string(MarkDown0, '\n', "", Lines),
    atomics_to_string(Lines, "\n%   ", MarkDown).

lpdoc2markdown(LpDoc, Dict, MarkDown, Options) :-
    string_codes(LpDoc, Codes),
    phrase((blanks,mantex(Parts, Dict)), Codes),
    flatten(Parts, Flat),
    atomics_to_string(Flat, String),
    with_output_to(string(MarkDown),
                   format_paragraph(String,
                                    [ text_align(justify)
                                    | Options
                                    ])).

mantex([], _) -->
    blanks, eos, !.
mantex([H|T], Dict) -->
    mantex_part(H, Dict),
    !,
    mantex(T, Dict).

mantex_part(Part, Dict) -->
    "@", csym(Name), "{", string(CCodes), "}",
    !,
    { atom_codes(Content, CCodes),
      make_part(Name, Content, Dict, Part)
    }.
mantex_part(Part, _) -->
    string_without("@", Codes),
    { string_codes(Part, Codes) }.


make_part(var, Name, Dict, Part) =>
    (   memberchk(Name=_, Dict)
    ->  Part = Name
    ;   Part = ['`', Name, '`']
    ).
make_part(pred, Pred, _, Part) =>
    Part = Pred.
make_part(em, Content, _, Part) =>
    Part = ['_', Content, '_'].
make_part(Cmd, Content, _, Part) =>
    print_message(warning, lpdoc(unknown_command(Cmd))),
    Part = [@, Cmd, '{', Content, '}'].

csym(Name) -->
    csym_code(H),
    csym_codes(T),
    { atom_codes(Name, [H|T]) }.

csym_code(H) -->
    [H],
    { code_type(H, csym) }.

csym_codes([H|T]) --> csym_code(H), !, csym_codes(T).
csym_codes([]) --> "".

		 /*******************************
		 *           SECTION		*
		 *******************************/

:- thread_local(doc_data/2).

replace_doc(section, Title, Replace) =>
    section_header(Title, SeqHdr),
    Replace = '$NODOT'('$TEXT'(SeqHdr)).
replace_doc(title, Title, Replace) =>
    Replace = '$NODOT'('$NOOP'("")),
    asserta(doc_data(title, Title)).
replace_doc(author, Author, Replace) =>
    Replace = '$NODOT'('$NOOP'("")),
    asserta(doc_data(author, Author)).
replace_doc(filetype, module, Replace) =>
    Replace = '$NODOT'('$NOOP'("")).
replace_doc(module, Comment, Replace) =>
    lpdoc2markdown(Comment, [], MarkDown, [width(72)]),
    doc_data(title, Title),
    doc_data(author, Author),
    format(string(Replace),
           '/** <module> ~w~n~n\c
           ~w~n~n\c
           @author ~w~n\c
           */~n~n',
           [ Title, MarkDown, Author ]).

section_header(Title, SeqHdr) :-
    with_output_to(string(SeqHdr), section_header(Title)).

section_header(Title) :-
    upcase_atom(Title, UTitle),
    format('\t\t /~|~`*t~31+~n\c
            \t\t *~|~t~w~t~30+*~n\c
            \t\t *~|~`*t~30+/~n~n\c', [UTitle]).

project_files(Files) :-
    findall(File, project_file(File), Files).

project_file(File) :-
    working_directory(CWD, CWD),
    source_file(File),
    sub_atom(File, 0, _, _, CWD),
    \+ file_base_name(File, 'refactor.pl').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Notes:

  - Is there a way to replace on all project files?
  - Can e.g., variable renaming be limited to a clause/predicate?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
