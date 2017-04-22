## Builds documentation: html or pdf 
## -- looks for a supported Scheme implementation

if type sagittarius ; then
  PROG="sagittarius -r7 -L . -L srfis/sagittarius "
else if type kawa ; then
  CLASSPATH=r7rs-libs.jar:$CLASSPATH
  PROG="kawa --r7rs "
else if type gosh ; then
  export GAUCHE_KEYWORD_IS_SYMBOL=1
  PROG="gosh -r7 -I . -I srfis/gauche/ "
else if type larceny ; then
  PROG="larceny -r7rs -program "
else if type chibi-scheme ; then
  PROG="chibi-scheme -I srfis/chibi/ "
else
  echo "Could not find a supported Scheme implementation"
  exit
fi
fi
fi
fi
fi

echo "creating data tables"
$PROG doc/create-tables.sps

# Depending on input arg, build that documentation
cd doc
if [ "$1" = "html" ] ; then
  echo "building html"
  sh mk-html.sh
else
  echo "building pdf"
  sh show-pdf.sh
fi
cd ..

