## Build pdf version of documentation
##
## Requires asciidoc's 'a2x' and 'sed' to be present
##
## Data tables must first be built by running:
## SCHEME_IMPLEMENTATION doc/create-tables.sps
## or use mk-doc.sh

## Optionally splices in alternative title page using 'pdftk', if present
## Optionally opens pdf in 'evince', if present

# ensure using [source,lisp], as a2x's highlighter does not know Scheme

for i in *.txt; do
    sed -i 's/\[source,scheme\]/\[source,lisp\]/g' $i
done

# Build pdf
a2x -f pdf --dblatex-opts "-P latex.output.revhistory=0" r7rs.txt

# use pdftk, if present, to splice in revised title page
if type pdftk ; then
  # Remove the title page
  pdftk r7rs.pdf cat 2-end output temp.pdf

  # Add the separate title page
  pdftk title.pdf temp.pdf cat output r7rs.pdf

  # Tidy up
  rm temp.pdf
fi

# If evince is present, show pdf
if type evince ; then
  evince r7rs.pdf
fi


