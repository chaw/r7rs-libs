# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc srfi-63.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="Homogeneous and Heterogeneous Arrays" --doc="srfi-63.html" ../srfis/chibi/srfi/63.sld
