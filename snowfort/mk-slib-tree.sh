# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-tree.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Implementation of COMMON LISP tree functions" --doc="slib-tree.html" ../slib/tree.sld
