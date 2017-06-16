# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-fourier.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Discrete Fourier Transform" --doc="slib-fourier.html" ../slib/fourier-transform.sld
