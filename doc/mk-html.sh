## Build html version of documentation
##
## Requires 'asciidoc' and 'sed' to be present
## optionally uses html-page-splitter
## -- install from ruby: gem install html_page_splitter
##
## Data tables must first be built by running:
## SCHEME_IMPLEMENTATION doc/create-tables.sps
## or use mk-doc.sh

# ensure highlighter is using Scheme for html output

for i in *.txt; do
    sed -i 's/\[source,lisp\]/\[source,scheme\]/g' $i
done

asciidoc --theme=volnitsky -a numbered -a toc r7rs.txt

# if html-page-splitter is present, then divide into separate pages
if type html-page-splitter ; then
  html-page-splitter -t "Documentation for R7RS Libraries" -p "Chapter" -d html r7rs.html
  rm r7rs.html
  # add text to title page
  sed -i.bkp '/<\/h1>/a <p>Written by Peter Lane</p>' html/index.html 
fi
