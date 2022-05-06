% Basic Event Calculus

%% Include the BASIC EVENT CALCULUS THEORY
#include 'bec_theory.incl'.

is_1_day_after(B, A) :- B #= A + 1.

%% based on the loan agreement example in Logical English
initiates(ends(A), it_is(B), _) :-
    is_1_day_after(B, A).
initiates(ends(1), is_liable_to_litigation(the_lender), A) :-
    holdsAt(requested_on(the_borrower, 1000, 2), A),
    not holdsAt(advanced_on(the_lender, 1000, _), 1).
initiates(ends(2), is_terminated(the_agreement), _) :-
    not holdsAt(requested_on(the_borrower, 1000, 2), 2).
initiates(requests(the_borrower, A), requested_on(the_borrower, A, B), B).
initiates(advances(the_lender, A), advanced_on(the_lender, A, B), B).
initiates(ends(_), is_potentially_defaulted(B), A) :-
    B=pays_to(C, D, E),
    is_due_on_from_to(D, A, C, E),
    holdsAt(it_is(F), F),
    not holdsAt(paid_to_on(C, D, E, _), F).
initiates(pays_to(A, B, C), paid_to_on(A, B, C, D), D).

is_due_on_from_to(550, 4, the_borrower, the_lender).
is_due_on_from_to(525, 5, the_borrower, the_lender).

%% Actions

happens(ends(1), 1).
happens(ends(2), 2).
happens(requests(the_borrower, 1000), 2).

?- holdsAt(F, T).



