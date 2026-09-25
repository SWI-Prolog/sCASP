# Sep 25, 2026

TODO:

  - The current sscasp naming is wrong.
    - In the end, ssCASP will simply become sCASP.
    - To facilitate easy switching, there will for now be library(scasp) and
      library(sscasp).  These should export the same API, i.e., scasp_*/*.
      Also internal code use scasp_ as prefix, not sscasp_.  Modules are for
      now still named sscap_ to avoid a conflict.
    - user:term_expansion/2 can check that the current module imports either
      scasp or sscasp by using predicate_property(sscasp(_,_), imported_from(M)).
      Use prolog_load_context(module, M) for finding the module we are compiling for.
  - Bring back scasp_assert, scasp_dynamic, etc.
  - Bring back sscasp -i
  - Include le_programs in tests
