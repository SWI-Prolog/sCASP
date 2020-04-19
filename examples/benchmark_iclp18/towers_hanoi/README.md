# Towers of Hanoi

## Clingo standard implementation

Known ASP encodings set a bound to the number of moves that can be
done, proposed in
([Gebser et al. 2008](https://www.cs.utexas.edu/users/vl/teaching/lbai/clingo_guide.pdf))
 for 7 disks and up to 126 movements.

```
clingo toh_standard_7.pl
clingo toh_standard_8.pl
clingo toh_standard_9.pl
```

## Clingo incremental implementation

ASP encoding by incrementing the number n of allowed movements (from
the [clingo 5.2.0](https://github.com/potassco/clingo) distribution) and proposerd in ([Gebser et al. 2014](https://arxiv.org/pdf/1405.3694.pdf))

```
clingo toh_incremental_7.pl
clingo toh_incremental_8.pl
clingo toh_incremental_9.pl
```

## s(CASP) implementation

s(CASP)’s top down approach can use a CLP-like control strategy to
implement the wellknown Towers of Hanoi algorithm. Predicate
hanoi(N,T) receives in N the number of disks and returns in T the
number of movements and a partial stable model which contains all the
movements and the time in which they have to be performed.
```
scasp hanoi_scasp_7.pl
scasp hanoi_scasp_8.pl
scasp hanoi_scasp_9.pl
```

