# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-line-io.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Line oriented input/output functions" --doc="slib-line-io.html" ../slib/line-io.sld
