:- module(scasp_output,
          [ print_model_term/2,
            connector/3                          % +Semantics,-Conn,+Options
          ]).

/** <module> Emit sCASP terms
*/

%!  print_model_term(+Term, +Options) is det.
%
%   Print a model element to the terminal.

:- det(print_model_term/2).

print_model_term(not(-Term), Options) =>
    connector(not, NotConn, Options),
    connector(negation, NegConn, Options),
    format('~w~w', [NotConn, NegConn]),
    print_plain(Term, likely, Options).
print_model_term(-Term, Options) =>
    connector(negation, NegConn, Options),
    format('~w', [NegConn]),
    print_plain(Term, false, Options).
print_model_term(not(Term), Options) =>
    connector(not, NotConn, Options),
    format('~w', [NotConn]),
    print_plain(Term, unlikely, Options).
print_model_term(Term, Options) =>
    print_plain(Term, true, Options).

print_plain(M:Term, Trust, Options) :-
    atom(M),
    !,
    ansi_format(fg('#888'), '~q:', [M]),
    print_plain(Term, Trust, Options).
print_plain(Term, Trust, _Options) :-
    style(Trust, Style),
    ansi_format(Style, '~p', [Term]).

style(true,     [bold]).
style(false,    [bold, fg(red)]).
style(likely,   []).
style(unlikely, [fg(red)]).

%!  connector(+Semantics, -Conn, +Options) is det.
%
%   Get an ASCII or Unicode connector string with the claimed Semantics.

:- det(connector/3).

connector(Semantics, Conn, Options) :-
    option(format(Format), Options, unicode),
    connector_string(Semantics, Format, Conn).

connector_string(implies,  ascii, ':-').
connector_string(and,      ascii, ',').
connector_string(negation, ascii, '-').
connector_string(not,      ascii, 'not ').

connector_string(implies,  unicode, '\u2190').   % <-
connector_string(and,      unicode, ' \u2227').  % /\
connector_string(negation, unicode, '\u00ac ').  % -
connector_string(not,      unicode, 'not ').

