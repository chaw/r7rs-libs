# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-string-port.txt

# Build the package
snow-chibi package --authors="Dorai Sitaram and Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Portable string ports" --doc="slib-string-port.html" ../slib/string-port.sld
