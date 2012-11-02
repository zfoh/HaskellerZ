
haskell_links.html: haskell_links.markdown
	pandoc haskell_links.markdown --standalone --smart --toc -t html -o haskell_links.html
