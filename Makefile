.PHONY: clean distclean

SWIPL?=swipl
SWIPL_PREFIX?=$(shell dirname $$(dirname $$(which ${SWIPL})))

all:	scasp sscasp prolog/scasp/main.qlf

scasp:
	${SWIPL} --no-pce --undefined=error -O -o scasp -c prolog/scasp/main.pl
sscasp:
	${SWIPL} --no-pce --undefined=error -O -o sscasp -c prolog/sscasp/main.pl
prolog/scasp/main.qlf:
	${SWIPL} qlf compile --include prolog/scasp/main.pl

check:	check_load
	${SWIPL} -g test_all -t halt test/test_all.pl

# Run the suite holding it to the *semantics* rather than to current
# behaviour.  Tests wrapped in `:- if(expect_semantics)' switch to the
# answer the reasoner ought to give, so the failures are the list of
# known defects still outstanding.  Expected to fail; it is a report, not
# a gate, so its exit status is ignored.
check_semantics:
	-${SWIPL} -Dscasp_expect=semantics -g 'test_all([jobs(1)])' -t halt \
		test/test_all.pl

# Verify consistency of main load points
check_load:
	${SWIPL} -q -l prolog/scasp.pl -g check --on-warning=status -t halt
	${SWIPL} -q -l prolog/scasp/main.pl -g check --on-warning=status -t halt
	${SWIPL} -q -p library=prolog -l prolog/sscasp.pl -g check --on-warning=status -t halt
	${SWIPL} -q -p library=prolog -l prolog/sscasp/main.pl -g check --on-warning=status -t halt

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
