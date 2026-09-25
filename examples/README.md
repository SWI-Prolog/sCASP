# Examples

The examples are written the way a user writes a program, against
`library(scasp)`.  Started with `-Dsscasp`, `library(scasp)` is the new
implementation instead (`prolog/scasp.pl`), so the same file runs against
either library:

    swipl examples/dyncall/birds.pl             # the old implementation
    swipl -Dsscasp examples/dyncall/birds.pl    # the new one

Run from a checkout rather than an installed pack, add `-p library=prolog`
so that `library(scasp)` is this tree's.

## Which run against both

`test/test_examples.pl` runs these against the library under test, so
`make check` notices when one stops answering what it did:

| | |
|---|---|
| `dyncall/birds.pl` | which birds fly, and which do not |
| `dyncall/pq.pl` | an even loop over negation |
| `dyncall/solve_findall.pl` | `findall/3` over a constraint |
| `rps.pl` | rock-paper-scissors with abducible throws |

## Which need `-Dsscasp`

These use what only the new library exports, for their output, and run
with `-Dsscasp` until `library(sscasp)` is `library(scasp)`:

| | |
|---|---|
| `dyncall/pas/PAS_case.pl` | `scasp_human/2`, the justification in words |
| `dyncall/pas/PAS_server.pl` | `html_model//2`, `html_justification//2` and the `sscasp` resource |
| `dyncall/http.pl` | the same, and `scasp_json/3`: an s(CASP) web service |

The other `.pl` files at the top of `examples/` are s(CASP) programs for
the executables, `scasp` and `sscasp`, not Prolog programs.
