# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-rationalize.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Find simplest number ratios" --doc="slib-rationalize.html" ../slib/rationalize.sld
