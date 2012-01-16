
handout.html: bytestring-talk.md
	pandoc -f markdown+lhs -s bytestring-talk.md -o handout.html

slides.html: bytestring-talk.md
	pandoc -f markdown+lhs -w slidy -s bytestring-talk.md --offline -o slides.html

all: handout.html slides.html

clean:
	rm -f handout.html
	rm -f slides.html

