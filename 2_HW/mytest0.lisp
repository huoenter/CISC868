(defun testing ()
(print-eval   '(
                (aver 3 5 8)
                (neg-test 5 -3 2)
                (neg-test 5 3 2)
                (neg-test 7 2 -6)
                (fact 5)
)))
