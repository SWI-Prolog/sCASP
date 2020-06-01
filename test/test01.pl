:- use_module('../src/scasp.pl').



list_tests(['pq.pl']).

main :-
    list_tests(Tests),
    member(Args,Tests),
    test(Args,Results),
    print(Results).

