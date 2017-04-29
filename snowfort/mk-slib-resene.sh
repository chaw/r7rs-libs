# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-resene.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Resene Color System" --doc="slib-resene.html" ../slib/resene.sld
