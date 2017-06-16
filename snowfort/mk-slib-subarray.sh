# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-subarray.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer and Radey Shouman" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Accessing parts of arrays" --doc="slib-subarray.html" ../slib/subarray.sld
