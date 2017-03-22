# Compile Kawa files into bin folder
OPTS="--r7rs -d bin "

cd srfis/kawa
# 27 needs compiling as a Java module
# 63 as a Scheme module
kawa -d ../../bin -C srfi/27.sld
kawa --r7rs -d ../../bin -C srfi/42.sld
kawa --r7rs -d ../../bin -C srfi/59.sld
kawa --r7rs -d ../../bin -C srfi/63.sld
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
