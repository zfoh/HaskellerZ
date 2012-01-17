all: handout.html slides.html

slides.html: bytestring-talk.md
	pandoc -f markdown+lhs -w slidy -s bytestring-talk.md --offline -o slides.html

handout.html: bytestring-talk.md
	pandoc -f markdown+lhs -s -S --toc bytestring-talk.md -o handout.html

clean:
	rm -f handout.html
	rm -f slides.html

