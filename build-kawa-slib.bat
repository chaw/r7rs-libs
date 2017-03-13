for %%f in (slib/*.sld) do (
  echo %%f
  kawa --r7rs -d bin -C "slib/%%~nf.sld"
)
