# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-uri.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Construct and decode Uniform Resource Identifiers" --doc="slib-uri.html" ../slib/uri.sld
