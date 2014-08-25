PANDOC = pandoc

all: Introduction.html criterion/CriterionIntro.html repa/RepaIntro.html accelerate/AccelerateIntro.html

%.html : %.lhs
	$(PANDOC) -f markdown+lhs -t slidy+lhs --self-contained --mathjax -s $< -o $@

clean:
	rm -f *.hi *.o

.PHONY: all clean
