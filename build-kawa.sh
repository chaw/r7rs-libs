# Compile Kawa files into bin folder
OPTS="--r7rs -d bin "

cd srfis/kawa
for file in srfi/*.sld
do
  kawa --r7rs -d ../../bin -C $file
done
cd ../..

# work through each directory in turn
for dir in slib robin rebottled pfds nltk; do
  for file in $dir/*.sld
  do
    kawa $OPTS -C $file
  done
done

# Create jar file to finish
cd bin
jar cf r7rs-libs.jar .
cd ..
mv bin/r7rs-libs.jar .
