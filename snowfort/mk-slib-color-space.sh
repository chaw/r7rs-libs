# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-color-space.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Color-space conversions" --doc="slib-color-space.html" ../slib/color-space.sld
