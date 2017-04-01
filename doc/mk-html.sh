asciidoc --theme=volnitsky -a numbered -a toc r7rs.txt
html-page-splitter -t "Documentation for R7RS Libraries" -p "Chapter" -d html r7rs.html
rm r7rs.html
# add text to title page
sed -i.bkp '/<\/h1>/a <p>Written by Peter Lane</p>' html/index.html 
