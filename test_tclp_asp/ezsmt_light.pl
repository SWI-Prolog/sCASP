


hour_dom(D) :- D .>. 0, D .<. 23.

switch :- not neg_switch.
neg_switch :- not switch.

lightOn :- switch, not am.

% :- not lightOn.

am :- not neg_am.
neg_am :- not am.

hour(X) :- hour_dom(X), X .>=. 12, not am.
hour(X) :- hour_dom(X), X .<. 12, am.

light_is(on) :- lightOn.
light_is(off) :- not lightOn.
