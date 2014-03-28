PANDOC = pandoc
FIGS=$(wildcard img/*.fig)
SVGS=$(FIGS:.fig=.svg)


all: final/pipes.html final/pipes-handout.html

img/%.svg: img/%.fig
	fig2dev -L svg $< $@

final/pipes.html: pipes.md $(SVGS)
	$(PANDOC) -f markdown -t slidy -s -S --toc --self-contained $< -o $@

final/pipes-handout.html: pipes.md
	$(PANDOC) -f markdown -t html -s -S --toc --self-contained $< -o $@

clean:
	rm -f img/*.svg final/*.html

.PHONY: all clean
