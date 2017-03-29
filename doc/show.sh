# Build pdf
a2x -f pdf --dblatex-opts "-P latex.output.revhistory=0"  r7rs.txt

# Show pdf
evince r7rs.pdf

