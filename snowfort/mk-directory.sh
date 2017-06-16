# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc directory.txt

# Build the package
snow-chibi --noimage package --authors="Peter Lane" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="Some useful directory functions" --doc="directory.html" ../robin/directory.sld
