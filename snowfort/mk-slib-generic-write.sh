# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-generic-write.txt

# Build the package
snow-chibi package --authors="Marc Feeley" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Generic write" --doc="slib-generic-write.html" ../slib/generic-write.sld
