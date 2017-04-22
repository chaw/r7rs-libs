# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc rebottled-pregexp.txt

# Build the package
snow-chibi package --authors="Dorai Sitaram" --maintainers="Peter Lane <peter@peterlane.info>" --version="20050502" --description="Dorai Sitaram's portable regular expressions" --doc="rebottled-pregexp.html" ../rebottled/pregexp.sld
