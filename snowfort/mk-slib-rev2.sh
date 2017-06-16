# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-rev2.txt

# Build the package
snow-chibi package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs-1" --description="Implementation of some R2RS procedures eliminated in subsequent versions" --doc="slib-rev2.html" ../slib/rev2-procedures.sld
