# Compile Kawa files into bin folder
OPTS="--r7rs -d bin "

cd srfis/kawa
for file in srfi/*.sld
do
  kawa --r7rs -d ../../bin -C $file
done
cd ../..

for file in slib/*.sld
do
  kawa $OPTS -C $file
done

for file in robin/*.sld
do
  kawa $OPTS -C $file
done

for file in pfds/*.sld
do
  kawa $OPTS -C $file
done

