# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc rebottled-pdf.txt

# Build the package
snow-chibi --noimage package --authors="Marc Battanyi and Bruce Butterfield" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.0.0" --description="Low level functions for generating PDF files" --doc="rebottled-pdf.html" ../rebottled/cl-pdf.sld ../rebottled/cl-pdf-utils.sld 
