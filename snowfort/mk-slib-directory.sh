# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-directory.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Directories" --doc="slib-directory.html" ../slib/directory.sld
