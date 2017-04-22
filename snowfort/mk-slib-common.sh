# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-common.txt

# Build the package
snow-chibi package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="SLIB core functions" --doc="slib-common.html" ../slib/common.sld
