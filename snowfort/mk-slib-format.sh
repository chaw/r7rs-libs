# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-format.txt

# Build the package
snow-chibi package --authors="Dirk Lutzebaeck, Ken Dickey, Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Common LISP text output formatter" --doc="slib-format.html" ../slib/format.sld
