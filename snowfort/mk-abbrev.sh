# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc abbrev.txt

# Build the package
snow-chibi package --authors="Peter Lane" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="Create unique abbreviations for a list of strings" --doc="abbrev.html" ../robin/abbrev.sld
