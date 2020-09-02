;-unit macroexpand -uses syntax
(include "inc/macro.scm")

(define (expr=? a b)
  (and (eqv? (expr-tag a)
             (expr-tag b))
       (equal? (expr-value a)
               (expr-value b))
       (all id (map expr=?
                    (sub-expr a)
                    (sub-expr b)))))

(define (macro-sym? x)
  (eqv? (string-ref (symbol->string x) 0)
        #\$))

(define (match-pattern pat e)
  (define (join-alist a b)
    (and b
         (foldl (lambda (alist pair)
                  (and alist
                       (let+ (k . v) pair
                         (let ((p (get k alist)))
                          (cond ((not p)
                                 (cons pair alist))
                                ((expr=? v p)
                                 alist)
                                (else #f))))))
                a
                b)))
  (define (match pat e)
    (cond ((null? pat)
           (and (null? e) '()))
          ((null? e) #f)
          ((symbol? pat)
           (cond ((eqv? pat '_)
                  '())
                 ((macro-sym? pat)
                  `((,pat . ,e)))
                 ((and (expr-is? e 'symbol)
                       (eqv? pat (expr-value e)))
                  '())
                 (else #f)))
          ((pair? pat)
           (and (expr-list? e)
                (cond
                  ((and (not (null? (cdr pat)))
                        (eqv? (cadr pat) '...))
                   (and (null? (cddr pat))
                        (cond
                          ((pair? (car pat))
                           (map (lambda (x)
                                  (cons (car x)
                                        (cdr x)))
                                (foldr (lambda (d dict)
                                         (map (lambda (p)
                                                (let+ (x . xs) p
                                                  (cons x
                                                        (cons (get x d)
                                                              xs))))
                                              dict))
                                       (map list (car pat))
                                       (map (cut match (car pat) <>)
                                            e))))
                          ((eqv? (car pat) '_) '())
                          ((macro-sym? (car pat))
                           `((,(car pat) . ,e)))
                          ((and (expr-is? e 'symbol)
                                (eqv? (car pat) (expr-value e)))
                           '())
                          (else #f))))
                  (else
                    (join-alist
                      (match (car pat) (car e))
                      (match (cdr pat) (cdr e)))))))
          (else #f)))
  (match pat e))

(define (transform-pattern pat)
  (cond ((null? pat) '())
        ((and (expr? pat)
              (expr-is? pat 'symbol))
         (expr-value pat))
        ((expr-list? pat)
         (map transform-pattern pat))))

(define (eval-macro-def e)
  (do
    ((: macro-env importing) <- (get-state))
    (let+ (: value sub) e
      (let ((patterns
              (map (lambda (x)
                     (let+ (pat body) x
                       (cons (transform-pattern pat)
                             body)))
                   (sub-expr e)))
            (name (car value)))
        (and (assert (not (in? name (builtin-forms)))
                     (syntax-error e "Name '{}' is reserved"
                                   name))
             (do
               (if (get name macro-env)
                 (return (fmt #t "Warning: redefinition of macro {}\n" name))
                 (return))
               (set-state
                 `(macro-env ,name)
                 `((clbk . ,(lambda (e)
                              (>>=
                                (find-pattern (cdr e)
                                              patterns)
                                (lambda (s)
                                  (return (and s (set-expr-pos s e)))))))
                   (patterns . ,patterns)
                   (imported . ,importing)))))))))

(define (eval-macro-defs e)
  (let+ (defs body) (split-by (cut expr-list-is? <> 'macro)
                              e)
    (do
      (for-each-m eval-macro-def
                  (map transform-macrodef defs))
      (return body))))

(define (transform-macrodef e)
  (define (valid-pattern? x)
    (or (and (expr-is? x 'struct)
             (all (disj (cut expr-is? <> 'symbol)
                        valid-pattern?)
                  (sub-expr x)))
        (and (expr-list? x)
             (all (disj (cut expr-is? <> 'symbol)
                        valid-pattern?)
                  x))))
  (let+ (sym name . clauses) e
    (and (assert (expr-is? name 'symbol)
                 (syntax-error
                   name "Expected symbol"))
         (make-expr
           (expr-file sym)
           (expr-line sym)
           (expr-col sym)
           'macrodef
           (list (expr-value name))
           (map (lambda (x)
                  (let+ (pattern . body) x
                    (and (assert (valid-pattern? pattern)
                                 (syntax-error
                                   pattern
                                   "Invalid pattern"))
                         (cons pattern body))))
                clauses)))))

(define (expand-macro-forms e)
  (do
    ((: macro-env) <- (get-state))
    (macro-names <- (return
                      (map car macro-env)))
    (letrec
      ((expand% (lambda (e)
                  (if (expr-list? e)
                    (if (and (expr? (car e))
                             (expr-is? (car e) 'symbol)
                             (in? (expr-value (car e))
                                  macro-names))
                      (do
                        (r <- (expand-macro e))
                        (inc-state 'count)
                        (return r))
                      (map-m expand% e))
                    (let+ (: sub) e
                      (if sub
                        (do
                          (sub <- (map-m expand% (sub-expr e)))
                          (return
                            (update-expr e 'sub sub)))
                        (return e))))))
       (loop (lambda (e)
               (do
                 (set-state 'count 0)
                 (e <- (expand% e))
                 ((: count) <- (get-state))
                 (if (= count 0)
                   (return e)
                   (loop e))))))
      (loop e))))

(define (get-macro name)
  (do
    ((: macro-env) <- (get-state))
    (return (or (get name macro-env)
                '()))))

(define (expand-macro e)
  (let ((file (expr-file e))
        (line (expr-line e))
        (col (expr-col e)))
    (do
      ((: clbk) <- (get-macro (expr-value (car e))))
      (if (not clbk)
        (return e)
        (do
          (r <- (clbk e))
          (if r
            (return r)
            (throw (syntax-error
                     e "Invalid macro form"))))))))

(define (find-pattern args x)
  (if (null? x)
    (return #f)
    (let+ (pat . body) (car x)
      (let ((dict (match-pattern pat args)))
       (if dict
         (return
           (subst-macro-body dict body))
         (find-pattern args (cdr x)))))))

(define set-expr-pos
  (case-lambda
    ((e s)
     (set-expr-pos
       e
       (expr-file s)
       (expr-line s)
       (expr-col s)))
    ((e file line col)
     (cond ((expr-list? e)
            (let ((b-line (expr-line e))
                  (b-col (expr-col e)))
              (map (lambda (x)
                     (set-expr-pos
                       x
                       file
                       (+ (- (or (expr-line x)
                                 b-line)
                             b-line)
                          line)
                       (+ (- (or (expr-col x)
                                 b-col)
                             b-col)
                          col)))
                   e)))
           ((expr? e)
            (update-expr
              e
              'file file
              'line line
              'col col))
           ((null? e) e)))))

(define (subst-macro-body dict body)
  (let ((names (map car dict)))
   (define (subst rest)
     (if (null? rest)
       rest
       (let+ (x . xs) rest
         (if (and (expr? x)
                  (expr-is? x 'symbol)
                  (in? (expr-value x)
                       names))
           (let ((v (get (expr-value x) dict)))
            (if (not v)
              (throw (syntax-error x "Invalid macro character"))
              (if (and (not (null? xs))
                       (expr? (car xs))
                       (expr-is? (car xs)
                                 'symbol)
                       (eqv? (expr-value (car xs))
                             '...))
                (append (set-expr-pos v x)
                        (subst (cdr xs)))
                (cons (set-expr-pos v x)
                      (subst xs)))))
           (if (expr-list? x)
             (cons (subst x) (subst xs))
             (cons x (subst xs)))))))
   (if (expr-list? body)
     (subst body)
     (unlist (subst (list body))))))

(define ($symbol-name e)
  (let+ (_ sym) e
    (and (assert (expr-is? sym 'symbol)
                 (syntax-error e "Expected symbol"))
         (return
           (make-expr (expr-file sym)
                      (expr-line sym)
                      (expr-col sym)
                      'string
                      (symbol->string (expr-value sym))
                      '())))))

(define ($makesym e)
  (let+ (_ . lst) e
    (and (assert (all (disj (cut expr-is? <> 'symbol)
                            (cut expr-is? <> 'string))
                      lst)
                 (syntax-error e "All arguments must be symbol/string"))
         (return
           (make-expr (expr-file e)
                      (expr-line e)
                      (expr-col e)
                      'symbol
                      (apply makesym (map expr-value lst))
                      '())))))

(define ($source-file e)
  (return
    (make-expr (expr-file e)
               (expr-line e)
               (expr-col e)
               'string
               (expr-file e)
               '())))

(define ($source-line e)
  (return
    (make-expr (expr-file e)
               (expr-line e)
               (expr-col e)
               'int
               (expr-line e)
               '())))

(define ($source-col e)
  (return
    (make-expr (expr-file e)
               (expr-line e)
               (expr-col e)
               'int
               (expr-col e)
               '())))

(define ($concat-str e)
  (let+ (_ . args) e
    (do
      (args <- (map-m expand-macro-forms args))
      (and (assert (all (conj expr?
                              (disj (cut expr-is?
                                         <>
                                         '(int string))))
                        args)
                   (syntax-error
                     e
                     "All arguments must be literal constants"))
           (return
             (make-expr
               (expr-file e)
               (expr-line e)
               (expr-col e)
               'string
               (apply concat (map expr-value args))
               '()))))))

(define ($fmt e)
  (let+ (_ . args) e
    (do
      (args <- (map-m expand-macro-forms args))
      (and (assert (all (conj expr?
                              (disj (cut expr-is?
                                         <>
                                         '(int string))))
                        args)
                   (syntax-error
                     e
                     "All arguments must be literal constants"))
           (return
             (make-expr
               (expr-file e)
               (expr-line e)
               (expr-col e)
               'string
               (apply fmt #f (map expr-value args))
               '()))))))

(define ($cstring e)
  (and
    (assert (= (length e) 2)
            (syntax-error
              e "Expected single argument"))
    (do
      (arg <- (expand-macro-forms (cadr e)))
      (and
        (assert (expr-is? arg 'string)
                (syntax-error
                  arg
                  "Expected string literal (got {})"
                  (expr-tag arg)))
        (return
          (make-expr-at
            e 'c-array #t
            (map (comp (cut make-expr-at e 'cast 'byte <>)
                       list
                       (cut make-expr-at e 'int <> '()))
                 (append (map char->integer
                              (string->list (expr-value arg)))
                         (list 0)))))))))

(define (init-macro-env)
  `(($symbol-name (clbk . ,$symbol-name))
    ($makesym (clbk . ,$makesym))
    ($source-file (clbk . ,$source-file))
    ($source-line (clbk . ,$source-line))
    ($source-col (clbk . ,$source-col))
    ($concat-str (clbk . ,$concat-str))
    ($fmt (clbk . ,$fmt))
    ($cstring (clbk . ,$cstring))))

(define (macro-expand e)
  (>>=
    (eval-macro-defs e)
    (lambda (e)
      (>>=
        (expand-macro-forms e)
        (lambda (e)
          (return (transform-sub-expr e)))))))

