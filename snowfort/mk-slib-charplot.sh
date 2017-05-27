# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-charplot.txt

# Build the package
snow-chibi --noimage package --authors="Aubrey Jaffer" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="Plotting histograms/graphs in characters" --doc="slib-charplot.html" ../slib/charplot.sld
