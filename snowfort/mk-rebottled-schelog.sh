# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc rebottled-schelog.txt

# Build the package
snow-chibi --noimage package --authors="Dorai Sitaram" --maintainers="Peter Lane <peter@peterlane.info>" --version="20150602" --description="Dorai Sitaram's Schelog: logic programming in Scheme" --doc="rebottled-schelog.html" ../rebottled/schelog.sld
