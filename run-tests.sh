# Run tests: chibi/gauche/kawa/larceny/sagittarius argument selects implementation
# For Kawa, the build script must first be run, which leaves r7rs-libs.jar 
#           in the current directory.

if [ "$1" = "kawa" ]; then
  export CLASSPATH=r7rs-libs.jar
  PROG="kawa --r7rs "
else if [ "$1" = "larceny" ]; then
  PROG="larceny -r7rs -program "
else if [ "$1" = "chibi" ]; then
  PROG="chibi-scheme -I srfis/chibi/ "
else if [ "$1" = "sagittarius" ]; then
  PROG="sagittarius -r7 -L . -L srfis/sagittarius "
else if [ "$1" = "gauche" ]; then
  export GAUCHE_KEYWORD_IS_SYMBOL=1
  PROG="gosh -r7 -I . -I srfis/gauche/ "
else
  echo "Unknown implementation"
  exit
fi
fi
fi
fi
fi

for dir in autodiff-tests nltk-tests pfds-tests rebottled-tests robin-tests slib-tests; do
  for file in $dir/*-test.sps
  do
    $PROG $file
  done
done

read -r -p "Test Weinholt? (y to run tests) " response
if [ "$response" = "y" ]; then
  for file in weinholt-tests/*-test.sps
  do
    $PROG $file
  done
fi

