# Build the documentation
asciidoc --theme=volnitsky -a numbered -a toc slib-filename.txt

# Build the package
snow-chibi --noimage package --authors="Radey Shouman" --maintainers="Peter Lane <peter@peterlane.info>" --version="SLIB-3b5-r7rs" --description="String matching for filenames (glob, a la BASH)" --doc="slib-filename.html" ../slib/filename.sld
