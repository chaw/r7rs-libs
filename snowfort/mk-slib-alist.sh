# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-alist.txt

# Build the package
snow-chibi package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Some functions for working with association lists" --doc="slib-alist.html" ../slib/alist.sld
