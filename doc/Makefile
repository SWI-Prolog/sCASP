PANDOC      ?= pandoc
LATEX       ?= lualatex
HIGHLIGHT   ?= tango

# The manual is written for GitHub: read it as GitHub markdown, so the
# Contents links (#1-installing ...) resolve, and make its one `#'
# heading the document title rather than a section.
PANDOC_IN    = -f gfm --shift-heading-level-by=-1

# Fonts come straight from fontconfig.  The Noto families are chosen
# for coverage: the manual uses ∉ ∈ ¬ • ― in running text and in code,
# which the LaTeX defaults (Latin Modern) do not have.
PANDOC_PDF   = --pdf-engine=$(LATEX) \
               -V mainfont="Noto Serif" \
               -V sansfont="Noto Sans" \
               -V monofont="Noto Sans Mono" \
               -V mathfont="Noto Sans Math" \
               -V geometry:margin=25mm \
               -V colorlinks=true \
               --highlight-style=$(HIGHLIGHT)

PANDOC_HTML  = -t html5 -s --embed-resources \
               --highlight-style=$(HIGHLIGHT)

# Each document is named once; `make manual' builds both formats of it.
DOCS = manual performance design-next
PDF  = $(DOCS:%=%.pdf)
HTML = $(DOCS:%=%.html)

all: $(DOCS)

$(DOCS): %: %.pdf %.html

pdf: $(PDF)
html: $(HTML)

%.pdf: %.md
	$(PANDOC) $< -o $@ $(PANDOC_IN) $(PANDOC_PDF)

%.html: %.md
	$(PANDOC) $< -o $@ $(PANDOC_IN) $(PANDOC_HTML)

clean:
	rm -f $(PDF) $(HTML)

.PHONY: all pdf html clean $(DOCS)
