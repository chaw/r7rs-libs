# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-topological-sort.txt

# Build the package
snow-chibi --noimage package --authors="Mikael Djurfeldt" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Topological sort" --doc="slib-topological-sort.html" ../slib/topological-sort.sld
