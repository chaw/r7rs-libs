# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-nbs-iscc.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="NBS/ISCC Color System" --doc="slib-nbs-iscc.html" ../slib/nbs-iscc.sld
