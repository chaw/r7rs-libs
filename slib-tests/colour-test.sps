;; Test access to colours through nbs-iscc, resene and saturate

(import (scheme base)
        (slib color)
        (slib nbs-iscc)
        (slib resene)
        (slib saturate)
        (srfi 64))

(test-begin "slib-nbs-iscc")

(test-assert (not (nbs-iscc "random colour")))
(test-assert (and (color? (nbs-iscc "dark yellow"))
                  (string=? "sRGB:171/145/68"
                            (color->string (nbs-iscc "dark yellow")))))

(test-end)

(test-begin "resene")

(test-assert (not (resene "random colour")))
(test-assert (and (color? (resene "woodburn"))
                  (string=? "sRGB:70/54/41"
                            (color->string (resene "woodburn")))))

(test-end)

(test-begin "saturate")

(test-assert (not (saturate "random colour")))
(test-assert (and (color? (saturate "red"))
                  (string=? "CIEXYZ:0.735484/0.264516/0"
                            (color->string (saturate "red")))))

(test-end)

