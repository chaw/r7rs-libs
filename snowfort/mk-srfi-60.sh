# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc srfi-60.txt

# Build the package
snow-chibi package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="Bit access and operations" --doc="srfi-60.html" ../srfis/chibi/srfi/60.sld
