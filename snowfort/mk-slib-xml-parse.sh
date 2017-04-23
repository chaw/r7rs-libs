# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-xml-parse.txt

# Build the package
snow-chibi package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="XML parsing and conversion to SXML" --doc="slib-xml-parse.html" ../slib/xml-parse.sld
