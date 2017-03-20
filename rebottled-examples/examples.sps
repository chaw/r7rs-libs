;; Examples from Schelog documentation to test and illustrate

(import (scheme base)
        (rebottled schelog)
        (slib format))

(define %knows
  (%rel ()
        (('Odysseus 'TeX))
        (('Odysseus 'Scheme))
        (('Odysseus 'Prolog))
        (('Odysseus 'Penelope))
        (('Penelope 'TeX))
        (('Penelope 'Prolog))
        (('Penelope 'Odysseus))
        (('Telemachus 'TeX))
        (('Telemachus 'calculus))))

(define %computer-literate
  (%rel (person)
        ((person)
         (%knows person 'TeX)
         (%knows person 'Scheme))
        ((person)
         (%knows person 'TeX)
         (%knows person 'Prolog))))

(define %computer-literate-2
  (lambda (person)
    (%and (%knows person
                  'TeX)
          (%or (%knows person
                       'Scheme)
               (%knows person
                       'Prolog)))))

(format #t "What does Odysseus know?~&")
(let loop ((he-knows (%which (what) (%knows 'Odysseus what))))
  (when he-knows
    (format #t "~a~&" he-knows)
    (loop (%more))))

(format #t "~%The following are computer literate~&")
(let loop ((literate (%which (who) (%computer-literate who))))
  (when literate
    (format #t "~a~&" literate)
    (loop (%more))))

(format #t "~%The following are computer literate (2)~&")
(let loop ((literate (%which (who) (%computer-literate-2 who))))
  (when literate
    (format #t "~a~&" literate)
    (loop (%more))))

(format #t "~%Testing combined goals~&")
(let loop ((result (%which (x)
                           (%and (%member x '(1 2 3))
                                 (%< x 3)))))
  (when result
    (format #t "~a~&" result)
    (loop (%more))))

(format #t "~%Things known (using bag) are: ~a~&"
        (%which (things-known)
                (%let (someone x)
                      (%bag-of x (%knows someone x)
                               things-known))))

(format #t "~%Things known (using set) are: ~a~&"
        (%which (things-known)
                (%let (someone x)
                      (%set-of x (%knows someone x)
                               things-known))))

(format #t "~%Things known, by person: ~&")
(let loop ((result (%which (someone things-known)
                           (%let (x)
                                 (%bag-of x
                                          (%free-vars (someone)
                                                      (%knows someone x))
                                          things-known)))))
  (when result
    (format #t "~a~&" result)
    (loop (%more))))


(define %factorial-1
  (%rel (x y x1 y1)
        ((0 1) !)
        ((x y) (%< x 0) ! %fail)
        ((x y) (%is x1 (- x 1))
               (%factorial-1 x1 y1)
               (%is y (* y1 x)))))

(define %factorial-2
  (%rel (x y x1 y1)
        ((0 1))
        ((x y) (%is x1 (- x 1))
               (%factorial-2 x1 y1)
               (%is y (* y1 x)))))

(format #t "Factorial output:~&")
(format #t "with cuts (factorial-1 10 n) = ~a~&" 
        (%which (n) (%factorial-1 10 n)))
(format #t "with no cuts (factorial-2 10 n) = ~a~&" 
        (%which (n) (%factorial-2 10 n)))

;; -----------------------------
;; puzzle.scm
;This is the puzzle solver described in Sterling & Shapiro, p. 214

;As S & S say, it is a "trivial" piece of code
;that successively solves each clue and query, which are expressed
;as Prolog goals and are executed with the meta-variable facility.

;The code in "real" Prolog, for comparison, is:
;
;  solve_puzzle(Clues, Queries, Solution) 
;                 :- solve(Clues), solve(Queries).
;
;  solve((Clue|Clues)) :- Clue, solve(Clues).
;  solve(()).

(define %solve-puzzle
  (%rel (clues queries solution)
        ((clues queries solution)
         (%solve clues)
         (%solve queries))))

(define %solve
  (%rel (clue clues)
        (((cons clue clues))
         clue 
         (%solve clues))
        (('()))))

;evaluate (solve-puzzle %puzzle) to get the solution to
;%puzzle.  Here %puzzle is a relation that is defined to
;hold for the three arguments clues, queries and solution=,
;iff they satisfy the constraints imposed by the puzzle.
;solve-puzzle finds an (the?) instantiation for the solution=
;variable.

(define solve-puzzle
  (lambda (%puzzle)
    (%let (clues queries)
          (%which (solution=)
                  (%and
                    (%puzzle clues queries solution=)
                    (%solve-puzzle clues queries solution=))))))

;; games.scm
;;This example is from Sterling & Shapiro, p. 214.
;;
;;The problem reads: Three friends came first, second and
;;third in a competition.  Each had a different name, liked a
;;different sport, and had a different nationality.  Michael
;;likes basketball, and did better than the American.  Simon,
;;the Israeli, did better than the tennis player.  The
;;cricket player came first.  Who's the Australian?  What
;;sport does Richard play?

(define person
  ;;a structure-builder for persons
  (lambda (name country sport)
    (list 'person name country sport)))

(define %games
  (%rel (clues queries solution the-men
               n1 n2 n3 c1 c2 c3 s1 s2 s3)
        ((clues queries solution)
         (%= the-men
             (list (person n1 c1 s1) (person n2 c2 s2) (person n3 c3 s3)))
         (%games-clues the-men clues)
         (%games-queries the-men queries solution))))

(define %games-clues
  (%rel (the-men clue1-man1 clue1-man2 clue2-man1 clue2-man2 clue3-man)
        ((the-men
           (list
             (%did-better clue1-man1 clue1-man2 the-men)
             (%name clue1-man1 'michael)
             (%sport clue1-man1 'basketball)
             (%country clue1-man2 'usa)

             (%did-better clue2-man1 clue2-man2 the-men)
             (%name clue2-man1 'simon)
             (%country clue2-man1 'israel)
             (%sport clue2-man2 'tennis)

             (%first the-men clue3-man)
             (%sport clue3-man 'cricket))))))

(define %games-queries
  (%rel (the-men man1 man2 aussies-name dicks-sport)
        ((the-men
           (list
             (%member man1 the-men)
             (%country man1 'australia)
             (%name man1 aussies-name)

             (%member man2 the-men)
             (%name man2 'richard)
             (%sport man2 dicks-sport))
           (list
             (list aussies-name 'is 'the 'australian)
             (list 'richard 'plays dicks-sport))))))

(define %did-better
  (%rel (a b c)
        ((a b (list a b c)))
        ((a c (list a b c)))
        ((b c (list a b c)))))

(define %name
  (%rel (name country sport)
        (((person name country sport) name))))

(define %country
  (%rel (name country sport)
        (((person name country sport) country))))

(define %sport
  (%rel (name country sport)
        (((person name country sport) sport))))

(define %first
  (%rel (car cdr)
        (((cons car cdr) car))))

(format #t "Solution of puzzle is: ~a~&"
        (solve-puzzle %games))

