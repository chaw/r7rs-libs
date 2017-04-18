# Install

These libraries have mostly been tested on Chibi, Kawa, Larceny and Sagittarius.  
Minor adjustments may be needed to make them work completely on other implementations of R7RS.

In most cases, installation is straightforward:

Download as a zip file and unpack, or clone the repository, using the 'Clone or Download'
button above.  If the directory containing the files is SCHEME_LIBS,
you must call your implementation with SCHEME_LIBS in its library path.  e.g.

    > chibi-scheme -I SCHEME_LIBS -I SCHEME_LIBS/srfis/chibi program.sps
    > larceny -path SCHEME_LIBS -r7rs -program program.sps
    > sagittarius -L SCHEME_LIBS -L SCHEME_LIBS/srfis/sagittarius program.sps

Note that Larceny has all required SRFIs to support the complete collection.  
For Chibi and Sagittarius, you also need to reference the implementation-specific SRFIs, 
in the directory SCHEME_LIBS/srfis/IMPLEMENTATION.

The -examples and -tests directories are not needed if you are only using the libraries in 
your programs.

## Kawa

For use in Kawa I recommend precompiling the libraries into class files.  The
script 'build-kawa', for bash or msdos, compiles all the libraries and packs
them into a single jar file: r7rs-libs.jar

Add this jar file to your CLASSPATH for use within Kawa.

Alternatively, for making the libraries permanently available, place the .jar file in your 
kawa/lib directory, and add the .jar file to the list in KAWA_EXTRA_PATH in the relevant 
kawa/bin script.

### Note: names with colons

Kawa uses the colon to access properties of a value, such as fields or methods, 
as explained in   https://www.gnu.org/software/kawa/Colon-notation.html
This sometimes causes problems in interpretation: one solution is to rename
names with colons on import, or use the | ... | vertical bar syntax when
referring to the library names.


# Testing

The collection comes with a large number of tests, which be run using the script "run-tests.sh".
Pass the name of your Scheme implementation as an argument: supported implementations are Chibi, 
Kawa, Larceny and Sagittarius.  i.e.

* $ sh run-tests.sh chibi
* $ sh run-tests.sh kawa
* $ sh run-tests.sh larceny
* $ sh run-tests.sh sagittarius

The tests can take a long time to run, depending on your implementation.  The final set of 
libraries, "Weinholt", can be particularly difficult, so the script asks before running them. 
I don't recommend testing Weinholt with Chibi or Larceny.

Apart from (slib format), which has three known issues, and Weinholt, all tests are expected 
to pass on all supported implementations.


# Documentation

Documentation is available online.  If you wish to rebuild it you will need:

* probably Linux
* asciidoc and, if you want pdf output, a2x (see http://www.methods.co.nz/asciidoc/)
* sed (which modifies the txt sources to suit the required highlighter for html or pdf)

Optionally (these steps are ignored, if the programs are not found):

* evince: to display the final pdf (or use your own pdf-viewer)
* html-page-splitter: a ruby program (gem install html_page_splitter) to divide
  a large html file into chapters (as I have placed online)
* pdftk: to splice in the alternative title page in the pdf (otherwise you get a2x's default)

To build the documentation, use the script "mk-doc.sh" with an input telling it which 
form of documentation to create:

* $ sh mk-doc.sh html
* $ sh mk-doc.sh pdf

Note: this will

1. run "doc/create-tables.sps" using any supported Scheme implementation it finds to create 
   files of data and tables for the documentation, using the source code for each library.
2. depending on your choice of output format, runs either "doc/mk-html.sh" or "doc/show-pdf.sh"

The finished documentation is left in the "doc" folder.  If you requested output as:

* html: one of
  * a single file "doc/r7rs.html", or
  * a collection of files in "doc/html/" (if you have "html-page-splitter")
* pdf: the final file is "doc/r7rs.pdf"


