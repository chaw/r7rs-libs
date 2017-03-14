
(import (scheme base)
        (slib tree)
        (srfi 64))

(test-begin "(slib tree)")

(define bar '(bar))
(test-equal (copy-tree (list bar 'foo))
            '((bar) foo))
(test-assert (not (eq? bar
                       (car (copy-tree (list bar 'foo))))))

(test-equal (substq 'tempest 'hurricane '(shakespeare wrote (the hurricane))) 
            '(shakespeare wrote (the tempest)))
(test-equal (substq 'foo '() '(shakespeare wrote (twelfth night)))
            '(shakespeare wrote (twelfth night . foo) . foo))
(test-equal (subst '(a . cons) '(old . pair)
                   '((old . spice) ((old . shoes) old . pair) (old . pair)))
            '((old . spice) ((old . shoes) a . cons) (a . cons)))

(test-end)

