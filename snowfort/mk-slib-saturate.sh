# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-saturate.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Saturated Color Dictionary" --doc="slib-saturate.html" ../slib/saturate.sld
