# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-strsearch.txt

# Build the package
snow-chibi package --authors="Oleg Kiselyov, Aubrey Jaffer and Steve VanDevender" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Functions for working with and searching within strings" --doc="slib-strsearch.html" ../slib/string-search.sld
