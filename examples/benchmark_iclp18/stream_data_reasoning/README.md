# Stream data reasoning

Let us assume that we deal with series of data items, some of which
may be contradictory. Moreover, different sources may give data a
different degree of trustworthiness which can make some pieces of
inconsistent data to be preferred. Lets us assume that `p(A)` and `q(A)`
are contradictory and we receive, from source _S1_, `p(A)` and, from
source _S2_, `q(a)`. We may decide that: (i) `p(A)` is __true__ because _S1_ is
more realiable; (ii) or if _S2_ is more realiable, `q(a)` is __true__, and any
value `not a` (i.e., _X \= a_) `p(A)` is also __true__; (iii) or, if both
sources are equally reliable, them we have (at least) two different
models: one where `q(a)` is __true__ and another where `p(A)` is __true__ (also
for _X=a_).


## s(CASP) implementation

```
scasp stream_data_reasoning.pl
```
