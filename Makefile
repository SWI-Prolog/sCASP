.PHONY: clean distclean

all:
	swipl --no-pce --undefined=error -O -o scasp -c prolog/scasp/main.pl

TEST_SCAP=swipl test/test_scasp.pl --passed

check:
	$(TEST_SCAP) test/all_programs test/all_programs/sasp
	$(TEST_SCAP) --dcc test/all_programs/dcc

ifneq ($(strip $(PREFIX)),)
install:
	@echo "Installing scasp in $(PREFIX)/bin/scasp"
	ln -sf "$(CURDIR)/scasp" "$(PREFIX)/bin/scasp"
else
install:
	@echo "Warning: No $$PREFIX defined."
	@echo "Warning: Too old Prolog or no writeable candidate"
	@echo "Warning: Could not install the scasp executable"
endif

clean:
	@-find . -name "*~" -type f -delete

distclean: clean
