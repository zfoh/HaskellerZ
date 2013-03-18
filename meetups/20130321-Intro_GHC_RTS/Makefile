all: handout.html slides.html

slides.html: intro_ghc_rts-talk.md
	pandoc -f markdown+lhs -w slidy -s intro_ghc_rts-talk.md --self-contained -o slides.html

handout.html: intro_ghc_rts-talk.md
	pandoc -f markdown+lhs -s -S --toc intro_ghc_rts-talk.md -o handout.html

clean:
	rm -f handout.html
	rm -f slides.html

enum-example:
	ghc -ddump-prep -fforce-recomp -dsuppress-all -O2 -ddump-to-file -ddump-stg -ddump-asm -ddump-cmm Enum.hs 
	
