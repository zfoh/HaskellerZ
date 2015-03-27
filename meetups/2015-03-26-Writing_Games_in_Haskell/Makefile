PANDOC = pandoc

all: final/games.html final/games-handout.html

final/games.html: games.md
	$(PANDOC) -f markdown -t slidy -s -S --toc --self-contained $< -o $@

final/games-handout.html: games.md
	$(PANDOC) -f markdown -t html -s -S --toc --self-contained $< -o $@

clean:
	rm -f final/*.html

.PHONY: all clean
