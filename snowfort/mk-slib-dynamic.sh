# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-dynamic.txt

# Build the package
snow-chibi --noimage package --authors="Andrew Wilcox" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Dynamic data type" --doc="slib-dynamic.html" ../slib/dynamic.sld
