# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-array-interpolate.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Interpolated array access" --doc="slib-array-interpolate.html" ../slib/array-interpolate.sld
