
slides.html: haskell_and_fp_in_industry.md img/*
	pandoc -f markdown+lhs -w slidy -s --self-contained haskell_and_fp_in_industry.md -o slides.html

handout.html: haskell_and_fp_in_industry.md img/*
	pandoc -f markdown+lhs -s -S --toc --self-contained haskell_and_fp_in_industry.md -o handout.html

all: handout.html slides.html

clean:
	rm -f handout.html
	rm -f slides.html

serve:
	python -mSimpleHTTPServer
