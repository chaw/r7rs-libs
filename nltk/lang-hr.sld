;;;
;;; Time-stamp: <2010-03-08 22:11:10 dcavar>
;;; encoding: UTF-8
;;;
;;; (C) 2009, 2010 Damir Ćavar <dcavar@unizd.hr>, Pavle Valerjev <valerjev@unizd.hr>
;;;
;;; The word list is automatically generated!
;;; Number of words: 299
;;;
;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the Creative Commons
;;; GNU Lesser General Public License as published by the
;;; Free Software Foundation; either version 2.1 of the License,
;;; or (at your option) any later version.
;;;
;;; The Scheme NLTK is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the Creative Commons GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser
;;; General Public License along with Web testing; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA
;;;
;;; Author: Damir Ćavar <dcavar@unizd.hr>,
;;;         Pavle Valerjev <valerjev@unizd.hr>
;;
;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Commentary:
;; 
;; TODO:
;; - word lists as Finite State Automata
;; - sample Context Free Grammar
;; - tokenizer

(define-library
  (nltk lang-hr)
  (export stopwords-list-hr
          stopwords-hash-hr)
  (import (scheme base)
          (nltk sequence))

  (begin

    (define stopwords-list-hr '("a" "ako" "ali" "bi" "bih" "bijah" "bijahu" "bijasmo" "bijaste" "bijaše" "bila" "bili" "bilo" "bio" "bismo" "biste" "biti" "bjeh" "bjehu" "bjesmo" "bjeste" "bješe" "bude" "budem" "budemo" "budete" "budeš" "budu" "bumo" "da" "do" "duž" "ga" "hoće" "hoćemo" "hoćete" "hoćeš" "hoću" "i" "iako" "ih" "ili" "ispod" "iz" "ja" "je" "jedna" "jedne" "jedno" "jer" "jesam" "jesi" "jesmo" "jest" "jeste" "jesu" "jim" "joj" "još" "ju" "k" "ka" "kada" "kako" "kao" "koja" "koje" "koji" "kojima" "koju" "kroz" "li" "me" "mene" "meni" "mi" "mimo" "mnom" "moj" "moja" "moje" "mojeg" "mojem" "mojim" "mojoj" "mojom" "moju" "mu" "na" "nad" "nakon" "nam" "nama" "nas" "naš" "naša" "naše" "našeg" "našem" "našim" "našoj" "našom" "našu" "ne" "nego" "neka" "neki" "nekog" "neku" "nema" "netko" "neće" "nećemo" "nećete" "nećeš" "neću" "nešto" "ni" "nije" "nikoga" "nikoje" "nikoju" "nisam" "nisi" "nismo" "niste" "nisu" "niti" "nje" "njega" "njegov" "njegova" "njegove" "njegovim" "njegovo" "njegovog" "njegovoj" "njegovom" "njegovu" "njemu" "njezin" "njezina" "njezine" "njezinim" "njezino" "njezinog" "njezinoj" "njezinom" "njezinu" "njih" "njihov" "njihova" "njihove" "njihovim" "njihovo" "njihovog" "njihovoj" "njihovom" "njihovu" "njim" "njima" "njoj" "njom" "nju" "no" "o" "od" "odmah" "on" "ona" "onaj" "one" "oni" "onim" "ono" "onog" "onoga" "onoj" "onom" "onomu" "ova" "ovaj" "ove" "ovim" "ovo" "ovog" "ovoga" "ovoj" "ovom" "ovomu" "pa" "pak" "po" "pod" "pored" "prije" "s" "sa" "sam" "samo" "se" "sebe" "sebi" "si" "smo" "ste" "su" "sve" "svi" "svog" "svoj" "svoja" "svoje" "svom" "ta" "tada" "taj" "tako" "te" "tebe" "tebi" "ti" "tim" "to" "tobom" "tog" "toga" "toj" "tom" "tome" "tomu" "tu" "tvoj" "tvoja" "tvoje" "tvojeg" "tvojem" "tvojim" "tvojoj" "tvojom" "tvoju" "u" "uz" "uza" "uzduž" "vam" "vama" "vas" "vaš" "vaša" "vaše" "vašeg" "vašem" "vašim" "vašoj" "vašom" "vašu" "već" "vi" "vrlo" "za" "zar" "će" "ćemo" "ćete" "ćeš" "ću" "što"))

    (define stopwords-hash-hr (sequence->hashtable stopwords-list-hr))

    ))

