# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc rebottled-pstk.txt
# Hard link the images to website, so will show in snow-fort
sed -i 's/images\//http:\/\/peterlane.info\/images\//g' rebottled-pstk.html

# Build the package
snow-chibi --noimage package --authors="Wolf-Dieter Busch and Nils Holm and Kenneth Dickey" --maintainers="Peter Lane <peter@peterlane.info>" --version="1.7.0" --description="Portable Scheme Interface to the Tk GUI Toolkit" --doc="rebottled-pstk.html" ../rebottled/pstk.sld 
