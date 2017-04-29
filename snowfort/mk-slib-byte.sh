# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-byte.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Arrays of small integers, not necessarily chars" --doc="slib-byte.html" ../slib/byte.sld
