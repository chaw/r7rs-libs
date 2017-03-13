for %%f in (srfi/*.sld) do (
  echo %%f
  kawa --r7rs -d bin -C "srfi/%%~nf.sld"
)

