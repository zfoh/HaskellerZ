all: handout.html slides.html

slides.html: commercial_haskell_primer.md img/*
	pandoc -f markdown+lhs -w slidy -s --self-contained commercial_haskell_primer.md -o slides.html

handout.html: commercial_haskell_primer.md img/*
	pandoc -f markdown+lhs -s -S --toc --self-contained commercial_haskell_primer.md -o handout.html

clean:
	rm -f handout.html
	rm -f slides.html

serve:
	python -mSimpleHTTPServer
