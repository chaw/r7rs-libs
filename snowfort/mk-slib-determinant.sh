# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-determinant.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Matrix Algebra" --doc="slib-determinant.html" ../slib/determinant.sld
