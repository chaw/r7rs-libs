# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-pretty-print.txt

# Build the package
snow-chibi package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Pretty printing" --doc="slib-pretty-print.html" ../slib/pretty-print.sld
