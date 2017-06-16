# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-pprint-file.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Pretty print a Scheme file" --doc="slib-pprint-file.html" ../slib/pprint-file.sld
