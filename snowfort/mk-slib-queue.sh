# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-queue.txt

# Build the package
snow-chibi --noimage package --authors="Andrew Wilcox" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Queue/Stack data structure" --doc="slib-queue.html" ../slib/queue.sld
