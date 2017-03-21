# Run tests: chibi/kawa/larceny argument selects implementation

if [ "$1" = "kawa" ]; then
  PROG="kawa --r7rs "
else if [ "$1" = "larceny" ]; then
  PROG="larceny -r7rs -program "
else if [ "$1" = "chibi" ]; then
  PROG="chibi-scheme -I srfis/chibi/ "
else
  echo "Unknown implementation"
  exit
fi
fi
fi

for dir in nltk-tests pfds-tests robin-tests slib-tests; do
  for file in $dir/*-test.sps
  do
    $PROG $file
  done
done

