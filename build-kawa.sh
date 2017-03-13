# Compile Kawa files into bin folder
OPTS="--r7rs -d bin "
kawa $OPTS -C srfi/*.sld
kawa $OPTS -C srfi-kawa/*.sld
kawa $OPTS -C slib/*.sld
kawa $OPTS -C robin/*.sld
