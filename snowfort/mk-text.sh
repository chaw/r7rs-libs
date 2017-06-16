# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc text.txt

# Build the package
snow-chibi package --authors="Peter Lane" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="A collection of functions for
working with strings or text documents, including similarity measures, a
stemmer and layout" --doc="text.html" ../robin/text.sld
