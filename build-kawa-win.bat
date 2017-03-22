cd srfis/kawa
# 27 needs compiling as a Java module
# 63 as a Scheme module
kawa -d ../../bin -C srfi/27.sld
kawa --r7rs -d ../../bin -C srfi/42.sld
kawa --r7rs -d ../../bin -C srfi/59.sld
kawa --r7rs -d ../../bin -C srfi/63.sld
cd ../..

for %%d in (nltk pfds rebottled robin slib) do (
  for %%f in (%%d/*.sld) do (
    echo %%d/%%f
    call kawa -d bin -C "%%d/%%~nf.sld"
  )
)

cd bin
jar cf r7rs-libs.jar .
cd ..
move bin\r7rs-libs.jar .

