.PHONY: clean distclean check check_scasp check_sscasp check_semantics check_load

SWIPL?=swipl
SWIPL_PREFIX?=$(shell dirname $$(dirname $$(which ${SWIPL})))

all:	scasp sscasp prolog/scasp/main.qlf

# The sources each executable is built from.  Without these the targets
# have no prerequisites, so editing a source never rebuilds and the
# binary is silently stale -- which is a bad way to find out that
# something behaves differently in a saved state.
#
# `pack.pl' is in both because `main.pl' reads the version out of it at
# *compile* time.

SCASP_SOURCES  := pack.pl prolog/scasp.pl \
		  $(wildcard prolog/scasp/*.pl prolog/scasp/*/*.pl)
SSCASP_SOURCES := pack.pl prolog/sscasp.pl $(wildcard prolog/sscasp/*.pl)

scasp:	$(SCASP_SOURCES)
	${SWIPL} -f none --no-pce --undefined=error -O -o scasp -c prolog/scasp/main.pl
sscasp:	$(SSCASP_SOURCES)
	${SWIPL} -f none --no-pce --undefined=error -O -o sscasp -c prolog/sscasp/main.pl
prolog/scasp/main.qlf: $(SCASP_SOURCES)
	${SWIPL} qlf compile --include prolog/scasp/main.pl

# The shared suites in test/ run against both implementations, each
# with its own suites from test/scasp/ or test/sscasp/.  Shared tests the
# new implementation does not pass yet are recorded in test/gaps.pl.
check:	check_load check_scasp check_sscasp

check_scasp:
	${SWIPL} -f none -g test_all -t halt test/test_all.pl

check_sscasp:
	${SWIPL} -f none -Dsscasp -g test_all -t halt test/test_all.pl

# Run the suite holding it to the *semantics* rather than to current
# behaviour.  Tests wrapped in `:- if(expect_semantics)' switch to the
# answer the reasoner ought to give, so the failures are the list of
# known defects still outstanding.  Expected to fail; it is a report, not
# a gate, so its exit status is ignored.
check_semantics:
	-${SWIPL} -f none -Dscasp_expect=semantics -g 'test_all([jobs(1)])' -t halt \
		test/test_all.pl

# Verify consistency of main load points
check_load:
	${SWIPL} -f none -q -l prolog/scasp.pl -g check --on-warning=status -t halt
	${SWIPL} -f none -q -l prolog/scasp/main.pl -g check --on-warning=status -t halt
	${SWIPL} -f none -q -p library=prolog -l prolog/sscasp.pl -g check --on-warning=status -t halt
	${SWIPL} -f none -q -p library=prolog -l prolog/sscasp/main.pl -g check --on-warning=status -t halt

ifneq ($(strip $(SWIPL_PREFIX)),)
install:
	@echo "Installing scasp in $(SWIPL_PREFIX)/bin/scasp"
	ln -sf "$(CURDIR)/scasp" "$(SWIPL_PREFIX)/bin/scasp"
	@echo "Installing sscasp in $(SWIPL_PREFIX)/bin/sscasp"
	ln -sf "$(CURDIR)/sscasp" "$(SWIPL_PREFIX)/bin/sscasp"
else
install:
	@echo 'Warning: No $$SWIPL_PREFIX defined.'
	@echo 'Warning: Too old Prolog or no writeable candidate'
	@echo 'Warning: Could not install the scasp executable'
endif

clean:
	@-find . -name "*~" -type f -delete

distclean: clean
	${RM} scasp sscasp prolog/scasp/main.qlf
