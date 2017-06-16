# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-chapter-order.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Chapter ordering" --doc="slib-chapter-order.html" ../slib/chapter-order.sld
