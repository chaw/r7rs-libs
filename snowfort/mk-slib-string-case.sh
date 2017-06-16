# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-string-case.txt

# Build the package
snow-chibi package --authors="Dirk Lutzebaeck, Ken Dickey, Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="String casing functions" --doc="slib-string-case.html" ../slib/string-case.sld
