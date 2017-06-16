# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-soundex.txt

# Build the package
snow-chibi package --authors="jjb and L.J.Buitinck" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Original Soundex algorithm" --doc="slib-soundex.html" ../slib/soundex.sld
