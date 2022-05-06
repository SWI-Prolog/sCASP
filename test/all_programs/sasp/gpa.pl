% From Baral Book pg 81
eligible(X) :- highGPA(X).
eligible(X) :- special(X), fairGPA(X).
-eligible(X) :- -special(X), -highGPA(X).
interview(X) :- not eligible(X), not -eligible(X).
fairGPA(john).
-highGPA(john).
?- interview(john).
