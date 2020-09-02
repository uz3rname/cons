;-uses config -uses compile
(import (chicken process-context))

(define (get-env-variable name)
  (let ((a (assoc name
                  (get-environment-variables))))
    (and a (cdr a))))

(main (argv))
