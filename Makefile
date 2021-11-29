.PHONY: clean lpdoc_clean

# --no-pce is only supported in the latest git build.  Remove
# when using an older version.  It only makes the program a
# little larger.
all:
	swipl --no-pce --undefined=error -O -o scasp -c prolog/scasp/main.pl

check:
	swipl test/test_scasp.pl test/programs test/programs/sasp
	swipl test/test_scasp.pl --dcc test/programs/dcc

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

clean: lpdoc_clean
	@$(MAKE) lpdoc_clean
	@-ciao clean-tree .
	@-find . -name "*~" -type f -delete
	@-find . -name "flymd.html" -type f -delete
	@-find . -name "flymd.md" -type f -delete
	@rm -fr scasp

lpdoc:
	@cd doc;\
	lpdoc -t html SETTINGS.pl >/dev/null 2>&1;\
	lpdoc --view -t html SETTINGS.pl;\
	cd ..;

lpdoc_clean:
	@cd doc;\
	lpdoc --realclean SETTINGS.pl;\
	cd ..;

# github:
#	git remote origin 'https://github.com/Xuaco/sCASP'
#	git
