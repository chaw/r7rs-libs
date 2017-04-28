# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-math-integer.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Mathematical functions restricted to exact integers" --doc="slib-math-integer.html" ../slib/math-integer.sld
