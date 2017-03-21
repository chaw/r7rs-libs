cd srfis/kawa
for %%f in (srfi/*.sld) do (
  echo %%f
  call kawa --r7rs -d ../../bin -C "srfi/%%~nf.sld"
)
cd ../..

for %%d in (nltk pfds rebottled robin slib) do (
  for %%f in (%%d/*.sld) do (
    echo %%d/%%f
    call kawa --r7rs -d bin -C "%%d/%%~nf.sld"
  )
)

cd bin
jar cf r7rs-libs.jar .
cd ..
move bin\r7rs-libs.jar .

