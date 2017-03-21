# Install

These libraries have mostly been tested on Chibi, Kawa and Larceny.  Minor adjustments 
may be needed to make them work completely on other implementations of R7RS.

In most cases, installation is straightforward:

Download and unpack these files into a directory, say SCHEME_LIBS.

Call your implementation with SCHEME_LIBS in its library path.  e.g.

    > chibi -I SCHEME_LIBS -I SCHEME_LIBS/srfis/chibi program.sps
    > larceny -path SCHEME_LIBS -r7rs -program program.sps

Note that Larceny has all required SRFIs to support the libraries.  For Chibi, you also need 
to reference the Chibi specific SRFIs, in the directory srfis/chibi.

The -examples and -tests directories are not needed if you are only using the libraries in 
your programs.

## Kawa

For use in Kawa I recommend precompiling the libraries into class files.  The script 'build-kawa',
for bash or msdos, compiles all the libraries and packs them into a single jar file: 
r7rs-libs.jar

Add this jar file to your CLASSPATH for use within Kawa.

Alternatively, for making the libraries permanently available, place the .jar file in your 
kawa/lib directory, and add the .jar file to the list in KAWA_EXTRA_PATH in the relevant 
kawa/bin script.

