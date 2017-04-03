# Scheme Libraries

Some libraries written for, or converted to run in, R7RS Scheme.  

Documentation in [pdf](http://peterlane.info/downloads/r7rs.pdf) and [html](http://peterlane.info/r7rs/html/index.html)

## NLTK

A repackaging of files from the Natural Language ToolKit:
https://sourceforge.net/projects/snltk/

* dfsa
* lang-en
* lang-hr 
* n-grams
* sequence
* vectorspace

(not cgi, as required R6RS libraries not present)

## PFDS

A repackaging of the Purely Functional Data Structures, originally written for
R6RS Scheme: https://github.com/ijp/pfds

* bounded-balance-tree 
* deque
* difference-list
* fingertree 
* hash-array-mapped-trie - all tests pass Chibi/Larceny - bad type crashes Kawa
* heap
* priority-search-queue
* queue
* sequence
* set

And, to support implementation:

* alist
* bitwise
* lazy-list
* list-helpers
* vector

## R6RS

Some R6RS libraries used to support other libraries.  These are taken from http://snow-fort.org

* base
* bytevectors
* fixnums

## Rebottled

Some smaller libraries, containing a single or few files, ported from various 
sources:

* cl-pdf - Port of a Common Lisp library for writing PDF files
* cl-pdf-utils
* pregexp - Dorai Sitaram's Portable regular expressions for Scheme
* quaternion - Dorai Sitaram's quaternion numbers
* schelog - Dorai Sitaram's logic-style programming in Scheme
  * not working on Kawa (due to call/cc)

## Robin

A set of libraries written for R7RS Scheme: 

* abbrev - creates a set of unambiguous abbreviations for strings (based on Ruby's Abbrev class)
* constants - some commonly used mathematical or scientific numbers
* directory - portable directory-handling functions
* disjoint-set - data structure to hold sets of items in disjoint sets
* logger - a logging framework (based on Ruby's Logger class)
* series - a mostly complete (though inefficient) version of Richard Waters' Lisp Series package
* srfi64-utils - some helper functions for SRFI 64 testing
* statistics - some descriptive statistical functions
* text - formatting, similarity measures etc for text data

## SLIB

SLIB is a long-established library containing around 30,000 lines of Scheme code, 
working with many Scheme implementations.  http://people.csail.mit.edu/jaffer/SLIB

This project aims to package as much of SLIB as possible into R7RS libraries, 
and ensure it works on several R7RS implementations. 

All libraries are imported as `(import (slib NAME))` in place of `(require 'NAME)`.
Function/variable names are preserved in most cases: documentation is available 
at http://people.csail.mit.edu/jaffer/slib

Version ported: 3b5 

Main changes are:

* (slib common) created to hold generally used definitions
* code uses SRFIs where possible
* defmacro not included

The following list of packages reflects the contents page of the slib
documentation, and gives some notes on any changes or choices made:

1: The Library System

(replaced with R7RS library system)

2: Universal SLIB Procedures

* vicinity: use (srfi 59)

Functions in configuration, input/output, system and miscellany are either no
longer needed (e.g. much of input/output is now present in R7RS) or have been
moved, as required, into `(slib common)`.

3: Scheme Syntax Extension Packages 

(These mostly appear to be included in R7RS Scheme or SRFIs.)

* yasos

4: Textual Conversion Packages

* precedence-parse
* format
  * format:symbol-case-conv format:iobj-case-conv format:max-iterations format:iteration-bounded
    available as parameters
  * three test cases fail (a known issue in this implementation)
  * alternatively use (srfi 28)
* printf
* scanf
* getopt
  * provided option-index/option-arg/option-name as parameters to access values
* comparse
* paramlst
* getparam
* html-form
* db->html
* http
  * http:byline is a parameter object
* html-for-each
* uri
* xml-parse
* generic-write
* object->string
* pretty-print
* pprint-file
* time-core
* time-zone
* posix-time
* common-lisp-time
* tzfile
* ncbi-dna
* schmooz

5: Mathematical libraries

* logical: use (srfi 60)
* modular
* math-integer
* math-real
* factor
* random: use (srfi 27)
* random-inexact
* dft
* crc
* charplot
  * provides (charplot:dimensions) as a parameter to get/change dimensions
* eps-graph
* solid
* color
* color-names
* color-space
* daylight
* root
* minimize
* limit
* commutative-ring
* determinant

6: Database Packages

* relational-database
* database
* database-interpolate
* database-commands
* within-database
  * without the macro support
* database-browse
* wt-tree

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
* collect
* dynamic
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
* hash-table: use (srfi 125) or (srfi 69)
* hash: or use (srfi 128)
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

(Remainder mostly in R7RS already: some added to `(slib common)` if necessary.)

### Required SRFIs

Some of the libraries require the following SRFIs (in place of related SLIB files):

* srfi 1   Lists
* srfi 13  Strings
* srfi 27  Random Bits
* srfi 59  Vicinities
* srfi 60  Integers as Bits
* srfi 63  Arrays
* srfi 69  Hash Tables 
* srfi 95  Sorting

## SRFIs

A few SRFIs are implemented here.  These fill gaps in those SRFIs provided by
some implementations and support the above libraries.  Provided SRFIs:

* srfi 27  for Kawa only: A wrapper around the JVM's Random class.
* srfi 42  simply the reference implementation (not needed)
* srfi 59  for Chibi and Kawa
* srfi 60  for Chibi
* srfi 63  SLIB's array.scm implemented as a srfi library
* srfi 64  for Chibi only: A partial implementation, wrapping (chibi test) -- required for running tests with Chibi

The SRFIs are organised in the 'srfis' folder, by implementation.

## Weinholt (in progress)

Repackaging of R6RS compression/cryptography libraries from https://github.com/weinholt/industria

* adler-32
* arcfour
* bitstream
* blowfish
  * Tests pass with Chibi and Larceny, not Kawa
* bytevector
* (des - tests fail in Kawa / overflow Larceny)
* hmac
* maths
* md5
* sha-1
* sha-2
* sliding-buffer
* strings

Following contain syntax-case and need extra work to convert: aes.sls, crc.sls, huffman.sls, pack.sls

which also affects: otr.sls; gzip.sls, inflate.sls, xz.sls, zip.sls; lzma2.sls, zlib.sls, openpgp.sls, ssh-public-key.sls, uuid.sls, x509.sls, buffer.sls, dns.sls, otr.sls, ssh.sls, tls.sls, internet.sls
