.PHONY: clean distclean

SWIPL?=swipl
SWIPL_PREFIX?=$(shell dirname $$(dirname $$(which ${SWIPL})))

all:	scasp prolog/scasp/main.qlf

scasp:
	${SWIPL} --no-pce --undefined=error -O -o scasp -c prolog/scasp/main.pl
prolog/scasp/main.qlf:
	${SWIPL} qlf compile --include prolog/scasp/main.pl

check:	check_load
	${SWIPL} -g test_all -t halt test/test_all.pl

# Verify consistency of main load points
check_load:
	${SWIPL} -q -l prolog/scasp.pl -g check --on-warning=status -t halt
	${SWIPL} -q -l prolog/scasp/main.pl -g check --on-warning=status -t halt

ifneq ($(strip $(SWIPL_PREFIX)),)
install:
	@echo "Installing scasp in $(SWIPL_PREFIX)/bin/scasp"
	ln -sf "$(CURDIR)/scasp" "$(SWIPL_PREFIX)/bin/scasp"
else
install:
	@echo 'Warning: No $$SWIPL_PREFIX defined.'
	@echo 'Warning: Too old Prolog or no writeable candidate'
	@echo 'Warning: Could not install the scasp executable'
endif

clean:
	@-find . -name "*~" -type f -delete

distclean: clean
	${RM} scasp prolog/scasp/main.qlf
