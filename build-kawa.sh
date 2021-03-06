## Compile Kawa files into bin folder
## and collect into a single jar file

# Reference kawa implementation to use
KAWA=kawa
OPTS="--r7rs -d bin "
export CLASSPATH=bin

# Remove existing files, if present
if [ -d "bin" ]; then
  rm -rf bin
fi
if [ -e "r7rs-libs.jar" ]; then
  rm r7rs-libs.jar
fi
# Make bin
mkdir bin

cd srfis/kawa
# 27 needs compiling as a Java module
${KAWA} -d ../../bin -C srfi/27.sld
cd ../..
for file in srfi/*.sld
do
  ${KAWA} $OPTS -C $file
done

# only one file from autodiff 
${KAWA} $OPTS -C autodiff/AD.sld

# work through each directory in turn
for dir in nltk pfds r6rs rebottled robin slib weinholt; do
  for file in $dir/*.sld
  do
    ${KAWA} $OPTS -C $file
  done
done

# Create jar file to finish
cd bin
jar cf r7rs-libs.jar .
cd ..
mv bin/r7rs-libs.jar .

# Tidy up
if [ -d "bin" ]; then
  rm -rf bin
fi
