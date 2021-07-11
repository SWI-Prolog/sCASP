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
    replace_sentence((:- pred Head # Comment),
                      '$NODOT'('$TEXT'(PlDoc)),
                     lpdoc2pldoc(Head, Comment, Dict, PlDoc),
                     [ variable_names(Dict)
                     | Options
                     ]),
    replace_sentence((:- doc(section, Title)),
                     '$NODOT'('$TEXT'(SeqHdr)),
                     section_header(Title, SeqHdr),
                     Options).

lpdoc2pldoc(Head, LpDoc, Dict, PlDoc) :-
    maplist(bind_varname, Dict),
    lpdoc2markdown(LpDoc, Dict, MarkDown),
    format(string(PlDoc),
           '%!  ~W~n\c
            %~n\c
            %   ~w~n~n',
           [ Head, [numbervars(true), spacing(next_argument)],
             MarkDown
           ]).

bind_varname(Name=Var) :-
    Var = ?'$VAR'(Name).

lpdoc2markdown(LpDoc, Dict, MarkDown) :-
    string_codes(LpDoc, Codes),
    phrase(mantex(Parts, Dict), Codes),
    flatten(Parts, Flat),
    atomics_to_string(Flat, String),
    with_output_to(string(MarkDown0),
                   format_paragraph(String,
                                    [ width(68),
                                      text_align(justify)
                                    ])),
    split_string(MarkDown0, '\n', "", Lines),
    atomics_to_string(Lines, "\n%   ", MarkDown).


mantex([], _) -->
    eos, !.
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
