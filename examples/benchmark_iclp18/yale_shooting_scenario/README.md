# Yale shooting scenario

Lets us compare the expressiveness of `s(CASP)` vs `ASP` + constraints
using the spoiling Yale shooting scenario
([Janhunen et al. 2017](https://arxiv.org/pdf/1707.04053.pdf)).

In this scenario we have an unloaded gun and three possible actions
load, shoot, and wait. If we load the gun, it becomes loaded. If we
shoot the gun and it was loaded for no more than 35 minutes, the
turkey is killed. Otherwise, the gun powder is spoiled. We are looking
for an executable plan such that:
* the turkey is killed within 100 minutes,
* considering that we are not allowed to shoot in the first 35
minutes.

## s(CASP) implementation

```
scasp yale_shooting_casp.pl
```
