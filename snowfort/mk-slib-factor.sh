# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-factor.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Factorization, prime test and generation" --doc="slib-factor.html" ../slib/factor.sld
