# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc statistics.txt

# Build the package
snow-chibi package --authors="Peter Lane" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="A library of functions to compute statistical or other information about sets of data" --doc="statistics.html" ../robin/statistics.sld
