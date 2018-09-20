
slides.html: tdd_unif.md img/*
	pandoc -f markdown+lhs -w slidy -s --self-contained tdd_unif.md -o slides.html

all: handout.html slides.html

handout.html: tdd_unif.md img/*
	pandoc -f markdown+lhs -s -S --toc --self-contained tdd_unif.md -o handout.html

clean:
	rm -f handout.html
	rm -f slides.html

serve:
	python -mSimpleHTTPServer
