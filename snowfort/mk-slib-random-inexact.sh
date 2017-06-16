# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-random-inexact.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Pseudo-Random inexact real numbers" --doc="slib-random-inexact.html" ../slib/random-inexact.sld
