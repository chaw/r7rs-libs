# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc disjoint-set.txt

# Build the package
snow-chibi package --authors="Peter Lane" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="A disjoint-set data structure" --doc="disjoint-set.html" ../robin/disjoint-set.sld
