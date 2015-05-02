FIGS=$(wildcard *.fig)
SVGS=$(FIGS:%.fig=final/%.svg)

all: final/slides.html

final/slides.html: slides.org $(SVGS)
	cp slides.org final/slides.org
	make -C final

final/%.svg: %.fig
	fig2dev -L svg $< $@

clean:
	rm -f final/*.svg final/*.html final/*.org

.PHONY: all clean
