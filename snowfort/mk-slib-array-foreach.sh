# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-array-foreach.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Applicative routines for arrays/matrices" --doc="slib-array-foreach.html" ../slib/array-for-each.sld
