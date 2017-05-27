# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-daylight.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Model of sun and sky colors" --doc="slib-daylight.html" ../slib/daylight.sld
