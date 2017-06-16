# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-modular.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Modular fixnum arithmetic" --doc="slib-modular.html" ../slib/modular.sld
