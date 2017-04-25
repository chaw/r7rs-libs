# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-scanf.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Implementation of POSIX-style formatted input" --doc="slib-scanf.html" ../slib/scanf.sld
