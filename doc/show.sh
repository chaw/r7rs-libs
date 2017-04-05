# ensure using [source,lisp], as a2x's highlighter does not know Scheme

for i in *.txt; do
    sed -i 's/[source,scheme]/[source,lisp]/g' $i
done

# Build pdf
a2x -f pdf --dblatex-opts "-P latex.output.revhistory=0"  r7rs.txt

# Remove the title page
pdftk r7rs.pdf cat 2-end output temp.pdf

# Add the separate title page
pdftk title.pdf temp.pdf cat output r7rs.pdf

# Tidy up
rm temp.pdf

# Show pdf
evince r7rs.pdf

