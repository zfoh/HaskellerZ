all: final/hflags.html final/hflags-handout.html

final/hflags.html: hflags.md
	mkdir -p final
	pandoc -t slidy -s $< -o $@

final/hflags-handout.html: hflags.md
	mkdir -p final
	pandoc -t html -s $< -o $@
