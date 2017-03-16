# Scheme Libraries

Some libraries written for or converted to run in R7RS Scheme.  
The converted libraries are from slib and srfis.

## Robin

A set of libraries written for R7RS Scheme: 

* abbrev - create set of unambiguous abbreviations for strings (like Ruby's Abbrev class)
* constants - some commonly used mathematical or scientific numbers
* logger - a logging framework (based on Ruby's Logger class)
* statistics - some descriptive statistical functions for lists
* text - formatting or manipulating text documents

## SLIB

SLIB is a long-established library containing around 30,000 lines of Scheme code, 
working with many Scheme implementations.  http://people.csail.mit.edu/jaffer/SLIB

This project aims to package as much of SLIB as possible into R7RS libraries, 
and ensure it works on several R7RS implementations. 

All libraries are imported as `(import (slib NAME))` in place of `(require 'NAME)`.
Function/variable names are preserved in most cases: documentation is available 
at http://people.csail.mit.edu/jaffer/slib

Version ported: 3b5 

Small changes are:

* (slib common) created to hold generally used definitions
* code uses SRFIs where possible

List of Libraries with notes on progress (no comment means done):

1: The Library System

(replaced with R7RS library system)

2: Universal SLIB Procedures

* vicinity: use (srfi 59)

Functions in configuration, input/output, system and miscellany are either no longer 
needed (e.g. much of input/output is now present in R7RS) or have been moved into (slib common).

3: Scheme Syntax Extension Packages 

(These mostly appear to be included in R7RS Scheme or SRFIs.)

* yasos: TODO?

TODO: defmacro - required?

4: Textual Conversion Packages

* precedence-parse
* format
  * alternatively use (srfi 28)
* printf
* scanf
* getopt
  * provided option-index/option-arg/option-name as parameters to access values
  * TODO: runs in Chibi and Kawa, not in Larceny
* html-form: TODO
* db->html: TODO?
* http: TODO?
* html-for-each
  * TODO: Fix warning on use of sscanf in case statement (line 452)
* uri
  * TODO: Fix warning on use of sscanf in case statement (line 452)
* xml-parse
* generic-write
* object->string: TODO?
* pretty-print
* pprint-file
* Time and Date: ? srfi 19
* tzfile
* ncbi-dma
* schmooz: TODO - in progress: needs srfi 59 

5: Mathematical libraries

* logical: use (srfi 60)
* modular
* math-integer: 
  * with quotient/remainder/modulo renamed as quotient-ei/remainder-ei/modulo-ei
* math-real: 
  * with abs renamed as real-abs
* factor
* random: use (srfi 27)
* random-inexact
* dft
* crc
* charplot
  * provides (charplot:dimensions) as a parameter to get/change dimensions
* eps-graph: TODO
* solid: TODO
* color: TODO
* color-space
* root
* minimize
* limit
* commutative-ring: TODO? (needs databases)
* determinant

6: Database Packages

* relational-database
* database: TODO - in progress
* database-interpolate
* database-commands
* within-database: TODO?
* database-browse
* wt-tree
  * tests pass with Chibi and Larceny, fails to compile with Kawa -- TODO

7: Other Packages

7.1: Data Structures

* arrays: use (srfi 63)
* subarray
* array-for-each
* array-interpolate
* alist
* byte
* byte-number
* pnm
* collect: TODO (requires yasos)
* dynamic
* hash-table: use (srfi 125) or (srfi 69)
* object
* priority-queue
* queue
* records: provided by R7RS

7.2: Sorting and Searching

* common-list-functions: use (srfi 1)
* tree
* chapter-order
* sort: use (srfi 132) or (srfi 95)
* topological-sort
* hash: use (srfi 128)
* space-filling
* hilbert-fill
* peano-fill
* sierpinski
* soundex
* string-search
* diff (sequence comparison)

7.3: Procedures

* coerce
* string-case
* metric-units

7.4: Standards Support

* rev2-procedures (some procedures from R2RS)

7.6: Systems Interface

* directory
  * Working with Chibi Scheme
  * Mostly working with Kawa - the pattern matching is not correct
  * Mostly working with Larceny - the pattern matching is not correct
    * and make-directory is exported (from primitives) but not into example

(Remainder mostly in R7RS already: some added to (slib common) if necessary.)

### Required SRFIs

Some of the libraries require the following SRFIs (in place of related SLIB files):

* srfi 1   Lists
* srfi 13  Strings
* srfi 27  Random Bits
* srfi 59  Vicinities
* srfi 60  Integers as Bits
* srfi 63  Arrays
* srfi 69  Hash Tables 


## SRFIs

A few SRFIs are implemented here.  These fill gaps in those SRFIs provided by
some implementations, mostly to use all of SLIB.  You should only install those
SRFIs which you need, and note that some of the SRFIs here are designed to work
only with particular implementations:

* srfi 27  for Kawa only: A wrapper around the JVM's Random class.
* srfi 42  simply the reference implementation
* srfi 63  SLIB's array.scm implemented as a srfi library

The srfis are organised in the 'srfis' folder, by implementation.

## R7RS Implementations

The libraries are currently tested on the following R7RS implementations.

### Kawa

A script is provided to compile all the files: 

    > sh build-kawa.sh

These put all the class files into ./bin  (equivalent .bat files provided for windows).

To make the libraries available for importing, add the ./bin directory to your CLASSPATH.
Alternatively, package up the class files into a jar file:

    > cd bin
    > jar cf r7rs-libs.jar slib srfi

then include the jar file on your classpath before launching kawa (e.g. in 
the kawa startup script: bin/kawa).

### Larceny

Add this directory to the search path when running programs, e.g.:

    > larceny -path .:~/r7rs-libs/ -r7rs -program examples/plot1.sps

### Chibi

Add this directory to the search path when running programs, e.g.:

    > chibi-scheme -I ~/r7rs-libs/:~/r7rs-libs/srfis/chibi/ examples/plot1.sps

### Cyclone

Cyclone compiles scheme code to self-contained executables.  It works with 
some of SLIB currently (there are limitations in number sizes, srfis etc).

Cyclone requires the library files to be compiled before compiling the 
programs.

e.g. to compile the examples/alist.sps program and run it:

    > cyclone slib/alist.sld
    > cyclone examples/alist.sps
    > examples/alist


