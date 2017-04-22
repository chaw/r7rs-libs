# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc logger.txt

# Build the package
snow-chibi package --authors="Peter Lane" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="A simple logging library for outputting messages while a program is running" --doc="logger.html" ../robin/logger.sld
