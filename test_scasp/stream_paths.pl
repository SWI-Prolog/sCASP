





path(A,B,T1,T2) :- conn(A,B,T1,T2).
path(A,C,T1,T2) :- conn(A,B,Ta1,Ta2), path(B,C,Tb1,Tb2).
