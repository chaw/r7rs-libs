# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-math-real.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs-1" --description="Mathematical functions restricted to real numbers" --doc="slib-math-real.html" ../slib/math-real.sld
