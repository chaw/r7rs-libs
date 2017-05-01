# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-wttree.txt

# Build the package
snow-chibi --noimage package --authors="Stephen Adams" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs-1" --description="Weight balanced trees" --doc="slib-wttree.html" ../slib/wt-tree.sld
