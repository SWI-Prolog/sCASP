% https://swish.swi-prolog.org/?code=https://raw.githubusercontent.com/samwalrus/scasp_haplotype/master/swish.swinb
%conflation.
conflation(2,0,1). %Heterozygous
conflation(2,1,0). %Heterozygous
conflation(0,0,0). %Homozygous
conflation(1,1,1). %Homozygous.

conflation_seq([],[],[]).
conflation_seq([G|Gs],[HA1|HAs],[HB1|HBs]):-
       conflation(G,HA1,HB1),
       conflation_seq(Gs,HAs,HBs).

genotype(_G,Genotype):-
    haplotype(Hap1),
    haplotype(Hap2),
    conflation_seq(Genotype,Hap1,Hap2).

haplotype(_X).

?- genotype(1,[2,1,2]),genotype(2,[1,2,1]).