:- module(ciao_auxiliar, [
    absolute_file_name/3,
    get_dir_name_ext/4,
    get_single_char/1,
    
    writef/1,
    writef/2,
    swritef/2,
    swritef/3,

    max/3,

    nb_setval/2,
    b_getval/2,
    b_setval/2,

    c_name/2,
    char_type/2
                     ]).


:- set_prolog_flag(multi_arity_warnings,off).


absolute_file_name(File, Abs, [relative_to(Rel)]) :-
    get_dir_name_ext(Rel, Dir, _, _),
    atom_concat(Dir, File, FFile),
    absolute_file_name(FFile, Abs).

get_dir_name_ext(File, Dir, Name, Ext) :-
    atom_codes(File,CFile),
    reverse(CFile, RF),
    get_dir_name_ext_(RF,RD,RN,RE),
    (
        RN == [] ->
        Ext = '',
        reverse(RE,CName), atom_codes(Name, CName)
    ;
        reverse(RE, CExt), atom_codes(Ext, CExt),
        reverse(RN, CName), atom_codes(Name, CName)
    ),
    reverse(RD, CDir), atom_codes(Dir, CDir).

get_dir_name_ext_([X|Xs], Dir, Name, [X|Es]) :-
    X \== 47,
    X \== 46,
    get_dir_name_ext_(Xs, Dir, Name, Es).
get_dir_name_ext_([X|Rest],Dir,Name,[46]) :-
    X \== 47,
    X == 46,
    get_dir_name_ext__(Rest,Dir,Name).
get_dir_name_ext_([X|Rest],[X|Rest],[],[]) :-
    X == 47.
get_dir_name_ext_([],[],[],[]).


get_dir_name_ext__([],[],[]).
get_dir_name_ext__([X|Xs],Dir,[X|N]) :-
    X \== 47,
    get_dir_name_ext__(Xs, Dir, N).
get_dir_name_ext__([X|Xs],[X|Xs],[]) :-
    X == 47.




get_single_char(X) :-
    get_char(X),
    (
        X \= '\n' ->
        get_char(_)
    ;
        true
    ).


writef(X) :-
    display(X).
writef(X,Y) :-
    format(X,Y).


swritef(Msg, Msg).
swritef(Msg, A, B) :-
    sformat(S, A, B),
    atom_codes(Msg, S).



c_name(Name, X) :- !,
    c_code_list(X, C),
    name(Name,C).
c_code_list([],[]).
c_code_list([X|Xs],[C|Cs]) :-
    char_code(X,C),
    c_code_list(Xs,Cs).



max(X,Y,X) :-
    X >= Y, !.
max(_,Y,Y).


nb_setval(X,Y) :- set_global(X,Y).
b_setval(X,Y) :- set_global(X,Y).
b_getval(X,Y) :- get_global(X,Y).
:- use_package(library(assertions)).
:- data set/2.
set_global(N, T) :- 
    nonvar(N),
    (retract_fact(set(N, _)) -> true ; true),
    asserta_fact(set(N, T)).
get_global(N, T) :-
    nonvar(N),
    current_fact(set(N, T1)), !,
    T = T1.


char_type(-1, end_of_file) :- !.        
char_type(X,Type) :-
    char_code(X,C),
    code_type(C,Type).

code_type(10,newline).
code_type(9,space).
code_type(10,space).
code_type(11,space).
code_type(12,space).
code_type(13,space).
code_type(32,space).
code_type(133,space).
code_type(160,space).
code_type(48,csym).
code_type(49,csym).
code_type(50,csym).
code_type(51,csym).
code_type(52,csym).
code_type(53,csym).
code_type(54,csym).
code_type(55,csym).
code_type(56,csym).
code_type(57,csym).
code_type(65,csym).
code_type(66,csym).
code_type(67,csym).
code_type(68,csym).
code_type(69,csym).
code_type(70,csym).
code_type(71,csym).
code_type(72,csym).
code_type(73,csym).
code_type(74,csym).
code_type(75,csym).
code_type(76,csym).
code_type(77,csym).
code_type(78,csym).
code_type(79,csym).
code_type(80,csym).
code_type(81,csym).
code_type(82,csym).
code_type(83,csym).
code_type(84,csym).
code_type(85,csym).
code_type(86,csym).
code_type(87,csym).
code_type(88,csym).
code_type(89,csym).
code_type(90,csym).
code_type(95,csym).
code_type(97,csym).
code_type(98,csym).
code_type(99,csym).
code_type(100,csym).
code_type(101,csym).
code_type(102,csym).
code_type(103,csym).
code_type(104,csym).
code_type(105,csym).
code_type(106,csym).
code_type(107,csym).
code_type(108,csym).
code_type(109,csym).
code_type(110,csym).
code_type(111,csym).
code_type(112,csym).
code_type(113,csym).
code_type(114,csym).
code_type(115,csym).
code_type(116,csym).
code_type(117,csym).
code_type(118,csym).
code_type(119,csym).
code_type(120,csym).
code_type(121,csym).
code_type(122,csym).
code_type(170,csym).
code_type(181,csym).
code_type(186,csym).
code_type(192,csym).
code_type(193,csym).
code_type(194,csym).
code_type(195,csym).
code_type(196,csym).
code_type(197,csym).
code_type(198,csym).
code_type(199,csym).
code_type(200,csym).
code_type(201,csym).
code_type(202,csym).
code_type(203,csym).
code_type(204,csym).
code_type(205,csym).
code_type(206,csym).
code_type(207,csym).
code_type(208,csym).
code_type(209,csym).
code_type(210,csym).
code_type(211,csym).
code_type(212,csym).
code_type(213,csym).
code_type(214,csym).
code_type(216,csym).
code_type(217,csym).
code_type(218,csym).
code_type(219,csym).
code_type(220,csym).
code_type(221,csym).
code_type(222,csym).
code_type(223,csym).
code_type(224,csym).
code_type(225,csym).
code_type(226,csym).
code_type(227,csym).
code_type(228,csym).
code_type(229,csym).
code_type(230,csym).
code_type(231,csym).
code_type(232,csym).
code_type(233,csym).
code_type(234,csym).
code_type(235,csym).
code_type(236,csym).
code_type(237,csym).
code_type(238,csym).
code_type(239,csym).
code_type(240,csym).
code_type(241,csym).
code_type(242,csym).
code_type(243,csym).
code_type(244,csym).
code_type(245,csym).
code_type(246,csym).
code_type(248,csym).
code_type(249,csym).
code_type(250,csym).
code_type(251,csym).
code_type(252,csym).
code_type(253,csym).
code_type(254,csym).
code_type(255,csym).
code_type(48,digit).
code_type(49,digit).
code_type(50,digit).
code_type(51,digit).
code_type(52,digit).
code_type(53,digit).
code_type(54,digit).
code_type(55,digit).
code_type(56,digit).
code_type(57,digit).
code_type(33,graph).
code_type(34,graph).
code_type(35,graph).
code_type(36,graph).
code_type(37,graph).
code_type(38,graph).
code_type(39,graph).
code_type(40,graph).
code_type(41,graph).
code_type(42,graph).
code_type(43,graph).
code_type(44,graph).
code_type(45,graph).
code_type(46,graph).
code_type(47,graph).
code_type(48,graph).
code_type(49,graph).
code_type(50,graph).
code_type(51,graph).
code_type(52,graph).
code_type(53,graph).
code_type(54,graph).
code_type(55,graph).
code_type(56,graph).
code_type(57,graph).
code_type(58,graph).
code_type(59,graph).
code_type(60,graph).
code_type(61,graph).
code_type(62,graph).
code_type(63,graph).
code_type(64,graph).
code_type(65,graph).
code_type(66,graph).
code_type(67,graph).
code_type(68,graph).
code_type(69,graph).
code_type(70,graph).
code_type(71,graph).
code_type(72,graph).
code_type(73,graph).
code_type(74,graph).
code_type(75,graph).
code_type(76,graph).
code_type(77,graph).
code_type(78,graph).
code_type(79,graph).
code_type(80,graph).
code_type(81,graph).
code_type(82,graph).
code_type(83,graph).
code_type(84,graph).
code_type(85,graph).
code_type(86,graph).
code_type(87,graph).
code_type(88,graph).
code_type(89,graph).
code_type(90,graph).
code_type(91,graph).
code_type(92,graph).
code_type(93,graph).
code_type(94,graph).
code_type(95,graph).
code_type(96,graph).
code_type(97,graph).
code_type(98,graph).
code_type(99,graph).
code_type(100,graph).
code_type(101,graph).
code_type(102,graph).
code_type(103,graph).
code_type(104,graph).
code_type(105,graph).
code_type(106,graph).
code_type(107,graph).
code_type(108,graph).
code_type(109,graph).
code_type(110,graph).
code_type(111,graph).
code_type(112,graph).
code_type(113,graph).
code_type(114,graph).
code_type(115,graph).
code_type(116,graph).
code_type(117,graph).
code_type(118,graph).
code_type(119,graph).
code_type(120,graph).
code_type(121,graph).
code_type(122,graph).
code_type(123,graph).
code_type(124,graph).
code_type(125,graph).
code_type(126,graph).
code_type(161,graph).
code_type(162,graph).
code_type(163,graph).
code_type(164,graph).
code_type(165,graph).
code_type(166,graph).
code_type(167,graph).
code_type(168,graph).
code_type(169,graph).
code_type(170,graph).
code_type(171,graph).
code_type(172,graph).
code_type(173,graph).
code_type(174,graph).
code_type(175,graph).
code_type(176,graph).
code_type(177,graph).
code_type(178,graph).
code_type(179,graph).
code_type(180,graph).
code_type(181,graph).
code_type(182,graph).
code_type(183,graph).
code_type(184,graph).
code_type(185,graph).
code_type(186,graph).
code_type(187,graph).
code_type(188,graph).
code_type(189,graph).
code_type(190,graph).
code_type(191,graph).
code_type(192,graph).
code_type(193,graph).
code_type(194,graph).
code_type(195,graph).
code_type(196,graph).
code_type(197,graph).
code_type(198,graph).
code_type(199,graph).
code_type(200,graph).
code_type(201,graph).
code_type(202,graph).
code_type(203,graph).
code_type(204,graph).
code_type(205,graph).
code_type(206,graph).
code_type(207,graph).
code_type(208,graph).
code_type(209,graph).
code_type(210,graph).
code_type(211,graph).
code_type(212,graph).
code_type(213,graph).
code_type(214,graph).
code_type(215,graph).
code_type(216,graph).
code_type(217,graph).
code_type(218,graph).
code_type(219,graph).
code_type(220,graph).
code_type(221,graph).
code_type(222,graph).
code_type(223,graph).
code_type(224,graph).
code_type(225,graph).
code_type(226,graph).
code_type(227,graph).
code_type(228,graph).
code_type(229,graph).
code_type(230,graph).
code_type(231,graph).
code_type(232,graph).
code_type(233,graph).
code_type(234,graph).
code_type(235,graph).
code_type(236,graph).
code_type(237,graph).
code_type(238,graph).
code_type(239,graph).
code_type(240,graph).
code_type(241,graph).
code_type(242,graph).
code_type(243,graph).
code_type(244,graph).
code_type(245,graph).
code_type(246,graph).
code_type(247,graph).
code_type(248,graph).
code_type(249,graph).
code_type(250,graph).
code_type(251,graph).
code_type(252,graph).
code_type(253,graph).
code_type(254,graph).
code_type(255,graph).
code_type(65,upper).
code_type(66,upper).
code_type(67,upper).
code_type(68,upper).
code_type(69,upper).
code_type(70,upper).
code_type(71,upper).
code_type(72,upper).
code_type(73,upper).
code_type(74,upper).
code_type(75,upper).
code_type(76,upper).
code_type(77,upper).
code_type(78,upper).
code_type(79,upper).
code_type(80,upper).
code_type(81,upper).
code_type(82,upper).
code_type(83,upper).
code_type(84,upper).
code_type(85,upper).
code_type(86,upper).
code_type(87,upper).
code_type(88,upper).
code_type(89,upper).
code_type(90,upper).
code_type(192,upper).
code_type(193,upper).
code_type(194,upper).
code_type(195,upper).
code_type(196,upper).
code_type(197,upper).
code_type(198,upper).
code_type(199,upper).
code_type(200,upper).
code_type(201,upper).
code_type(202,upper).
code_type(203,upper).
code_type(204,upper).
code_type(205,upper).
code_type(206,upper).
code_type(207,upper).
code_type(208,upper).
code_type(209,upper).
code_type(210,upper).
code_type(211,upper).
code_type(212,upper).
code_type(213,upper).
code_type(214,upper).
code_type(216,upper).
code_type(217,upper).
code_type(218,upper).
code_type(219,upper).
code_type(220,upper).
code_type(221,upper).
code_type(222,upper).
code_type(33,punct).
code_type(34,punct).
code_type(35,punct).
code_type(36,punct).
code_type(37,punct).
code_type(38,punct).
code_type(39,punct).
code_type(40,punct).
code_type(41,punct).
code_type(42,punct).
code_type(43,punct).
code_type(44,punct).
code_type(45,punct).
code_type(46,punct).
code_type(47,punct).
code_type(58,punct).
code_type(59,punct).
code_type(60,punct).
code_type(61,punct).
code_type(62,punct).
code_type(63,punct).
code_type(64,punct).
code_type(91,punct).
code_type(92,punct).
code_type(93,punct).
code_type(94,punct).
code_type(95,punct).
code_type(96,punct).
code_type(123,punct).
code_type(124,punct).
code_type(125,punct).
code_type(126,punct).
code_type(161,punct).
code_type(162,punct).
code_type(163,punct).
code_type(164,punct).
code_type(165,punct).
code_type(166,punct).
code_type(167,punct).
code_type(168,punct).
code_type(169,punct).
code_type(171,punct).
code_type(172,punct).
code_type(173,punct).
code_type(174,punct).
code_type(175,punct).
code_type(176,punct).
code_type(177,punct).
code_type(180,punct).
code_type(182,punct).
code_type(183,punct).
code_type(184,punct).
code_type(187,punct).
code_type(191,punct).
code_type(215,punct).
code_type(247,punct).
code_type(97,lower).
code_type(98,lower).
code_type(99,lower).
code_type(100,lower).
code_type(101,lower).
code_type(102,lower).
code_type(103,lower).
code_type(104,lower).
code_type(105,lower).
code_type(106,lower).
code_type(107,lower).
code_type(108,lower).
code_type(109,lower).
code_type(110,lower).
code_type(111,lower).
code_type(112,lower).
code_type(113,lower).
code_type(114,lower).
code_type(115,lower).
code_type(116,lower).
code_type(117,lower).
code_type(118,lower).
code_type(119,lower).
code_type(120,lower).
code_type(121,lower).
code_type(122,lower).
code_type(170,lower).
code_type(181,lower).
code_type(186,lower).
code_type(223,lower).
code_type(224,lower).
code_type(225,lower).
code_type(226,lower).
code_type(227,lower).
code_type(228,lower).
code_type(229,lower).
code_type(230,lower).
code_type(231,lower).
code_type(232,lower).
code_type(233,lower).
code_type(234,lower).
code_type(235,lower).
code_type(236,lower).
code_type(237,lower).
code_type(238,lower).
code_type(239,lower).
code_type(240,lower).
code_type(241,lower).
code_type(242,lower).
code_type(243,lower).
code_type(244,lower).
code_type(245,lower).
code_type(246,lower).
code_type(248,lower).
code_type(249,lower).
code_type(250,lower).
code_type(251,lower).
code_type(252,lower).
code_type(253,lower).
code_type(254,lower).
code_type(255,lower).


    
