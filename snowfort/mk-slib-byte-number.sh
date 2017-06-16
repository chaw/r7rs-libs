# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-byte-number.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Byte integer and IEEE floating-point conversions" --doc="slib-byte-number.html" ../slib/byte-number.sld
