# Run tests: chibi/gauche/kawa/larceny/sagittarius argument selects implementation
# For Kawa, r7rs-libs.jar must be built and on the CLASSPATH
#           (similarly, r7rs-large.jar)

if [ "$1" = "kawa" ]; then
  export CLASSPATH=r7rs-libs.jar
  PROG="kawa --r7rs -f "
  # Test srfi 27 for Kawa
  $PROG "srfis/kawa/srfi-27-test.sps"
else if [ "$1" = "larceny" ]; then
  PROG="larceny -r7rs -program "
else if [ "$1" = "chibi" ]; then
  PROG="chibi-scheme -I srfis/chibi/ "
else if [ "$1" = "sagittarius" ]; then
  PROG="sagittarius -r7 -L . "
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

for dir in autodiff-tests nltk-tests pfds-tests rebottled-tests robin-tests slib-tests srfi-tests; do
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

