PANDOC = pandoc

all: final/ghchack.html final/ghchack-handout.html

final/ghchack.html: ghchack.md $(SVGS)
	$(PANDOC) -f markdown -t slidy -s -S --toc --self-contained $< -o $@

final/ghchack-handout.html: ghchack.md
	$(PANDOC) -f markdown -t html -s -S --toc --self-contained $< -o $@

clean:
	rm -f final/*.html

.PHONY: all clean
