# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-color.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Color data type" --doc="slib-color.html" ../slib/color.sld
