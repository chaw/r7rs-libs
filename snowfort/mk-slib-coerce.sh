# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-coerce.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Implementation of COMMON-LISP COERCE and TYPE-OF" --doc="slib-coerce.html" ../slib/coerce.sld
