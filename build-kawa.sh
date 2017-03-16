# Compile Kawa files into bin folder
OPTS="--r7rs -d bin "
cd srfis/kawa
kawa --r7rs -d ../../bin -C srfi/*.sld
cd ../..
kawa $OPTS -C slib/*.sld
kawa $OPTS -C robin/*.sld
