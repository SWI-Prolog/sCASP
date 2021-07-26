.PHONY: clean lpdoc_clean


compile_scasp:
	swipl -O -o scasp -c prolog/scasp/main.pl

check:
	swipl test/test_scasp.pl

qcheck:
	swipl test/test_scasp.pl -q

check-all:
	swipl test/test_scasp.pl test/programs test/programs/sasp


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
