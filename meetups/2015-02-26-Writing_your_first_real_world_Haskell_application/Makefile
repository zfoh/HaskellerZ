# TODO: Make slides and handout --self-contained
all: handout.html slides.html

slides.html: real_world_haskell_intro.markdown fig/*
	pandoc -f markdown+lhs -w slidy -s --self-contained real_world_haskell_intro.markdown -o slides.html

handout.html: real_world_haskell_intro.markdown fig/*
	pandoc -f markdown+lhs -s -S --toc --self-contained real_world_haskell_intro.markdown -o handout.html

clean:
	rm -f handout.html
	rm -f slides.html
