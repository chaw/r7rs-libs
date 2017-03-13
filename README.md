# Scheme Libraries

Some libraries converted to run in R7RS Scheme, mostly from slib and srfis.

## Implementations

### Kawa

A script is provided to compile all the files: > sh build-kawa.sh
These put all the class files into ./bin
Add the ./bin directory to your CLASSPATH to make the libraries available 
for import into R7RS programs.

### Larceny

Add this directory to the search path when running programs, e.g.:

    larceny -path .:~/Software/r7rs-libs/ -r7rs -program examples/plot1.sps

### Chibi

Add this directory to the search path when running programs, e.g.:

    chibi-scheme -I ~/Software/r7rs-libs/ examples/plot1.sps

### Cyclone

Cyclone compiles scheme code to self-contained executables.  It works with 
some of SLIB currently (there are limitations in number sizes, srfis etc).

Cyclone requires the library files to be compiled before compiling the 
programs.

e.g. to compile the examples/alist.sps program and run it:

    cyclone slib/alist.sld
    cyclone examples/alist.sps
    examples/alist


## SLIB

SLIB is a long-established library of around 30,000 lines of Scheme code, 
working with many Scheme implementations.  http://people.csail.mit.edu/jaffer/SLIB

All libraries are imported as (import (slib NAME)) in place of (require 'NAME).
Function/variable names are preserved in most cases.

Version ported: 3b5 

Small changes are:

* Assumes bignums/complex/reals etc supported by implementations 
  (no checking with flag) - this feature needs adding back in
* (slib common) created to hold generally used definitions

List of Libraries with notes on progress (no comment means done):

1. Textual Conversion Packages

* precedence-parse
* format
  * alternatively use (srfi 28)
* printf
* scanf
* getopt: TODO
* html-form: TODO
* db->html: TODO?
* http: TODO?
* html-for-each: TODO
* uri
  * TODO: Fix warning on use of sscanf in case statement (line 452)
* xml-parse: TODO
* generic-write
* object->string: TODO?
* pretty-print
* pprint-file
* Time and Date: ? srfi 19
* ncbi-dma
* schmooz: TODO?

2. Mathematical libraries

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
  * provided (charplot:dimensions) and (charplot:dimensions-set! ) to get/change dimensions
* eps-graph: TODO
* solid: NOT PLANNED
* color: NOT PLANNED
* root
* minimize
* limit
* commutative-ring: TODO? (needs databases)
* determinant

3. Database Packages

* database: TODO?
* wt-tree
  * tests pass with Larceny, fails to compile with Kawa -- TODO

4. Data Structures

* arrays: use (srfi 63)
* subarray
* array-for-each
* array-interpolate
* alist
* byte
* byte-number
* matfile: NOT PLANNED
* pnm
* collect: TODO (requires yasos)
* dynamic: TODO?
* hash-table: use (srfi 125)
* object
* priority-queue
* queue
* records: provided by R7RS

5. Searching and Sorting

* common-list-functions: use (srfi 1)
* tree
* chapter-order
* sort: use (srfi 95)
* topological-sort
* hash: use (srfi 128)
* space-filling
* hilbert-fill
* peano-fill
* sierpinski
* soundex
* string-search
* diff (sequence comparison)

6. Procedures

* coerce
* string-case
* metric-units

### Required SRFIs

Some of the libraries require the following SRFIs:

* srfi 1   Lists
* srfi 13  Strings
* srfi 27  Random Bits
* srfi 60  Integers as Bits
* srfi 63  Arrays
* srfi 69  Hash Tables 


## SRFIs

A few SRFIs are implemented here.  These fill gaps in those SRFIs provided 
by some implementations, mostly to use all of SLIB.  Note that some of the 
SRFIs here are designed to work only with some implementations:

* srfi 27  for Kawa only: A wrapper around the JVM's Random class.
* srfi 42  simply the reference implementation
* srfi 63  SLIB's array.scm implemented as a srfi library

