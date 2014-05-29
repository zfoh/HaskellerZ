GHC = ghc
PANDOC = pandoc

all: final/make-you-a-monad.html final/make-you-a-monad-handout.html

final/make-you-a-monad.html: make-you-a-monad.md $(SVGS)
	$(PANDOC) -f markdown+lhs -t slidy -s -S --toc --self-contained $< -o $@

final/make-you-a-monad-handout.html: make-you-a-monad.md
	$(PANDOC) -f markdown+lhs -t html -s -S --toc --self-contained $< -o $@

clean:
	 rm -f final/*.html
