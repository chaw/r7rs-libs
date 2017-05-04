# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-minimize.txt

# Build the package
snow-chibi --noimage package --authors="Lars Arvestad" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Finds minimum value of a function" --doc="slib-minimize.html" ../slib/minimize.sld

