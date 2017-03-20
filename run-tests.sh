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

for file in nltk-tests/*-test.sps
do
  $PROG $file
done

for file in pfds-tests/*-test.sps
do
  $PROG $file
done

for file in robin-tests/*-test.sps
do
  $PROG $file
done

for file in slib-tests/*-test.sps
do
  $PROG $file
done

