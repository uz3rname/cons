;-unit types -uses prelude
(include "inc/macro.scm")
(include "inc/typedefs.scm")

(define (init-type-env)
  `((type-table . ,type-defs)
    (numeric-tower #f byte int16 int32 int real)))

(define (builtin-types)
  (map car (get 'type-table (init-type-env))))

(define (arrow? x)
  (eqv? x '->))

(define (hashrock? x)
  (eqv? x '=>))

(define (colon? x)
  (eqv? x '|:|))

(define (asterisk? x)
  (eqv? x '*))

(define (ellipsis? x)
  (eqv? x '...))

(define (special-type-sym? x)
  (or (arrow? x)
      (hashrock? x)
      (colon? x)
      (ellipsis? x)))

(define (xplode sym)
  (-> sym
    symbol->string
    string->list))

(define (template-arg? x)
  (and (symbol? x)
       (let+ (a . xs) (xplode x)
        (and (char-alphabetic? a)
             (all char-numeric? xs)))))

(define (template-arg-weight x)
  (let* ((s (symbol->string x))
         (ch (char->integer (string-ref s 0)))
         (l (string-length s))
         (n (and (> l 1)
                 (string->number (substr s 1 l)))))
    (+ (* ch 100)
       (or (and n (* n 10)) 0))))

(define (template-arg< x y)
  (< (template-arg-weight x)
     (template-arg-weight y)))

(define (template-args t)
  (and (list? t)
       (-> t
         flatten
         (filter template-arg? <>)
         (sort <> template-arg<)
         unique)))

(define (make-template-arg x)
  (-> (char->integer #\a)
    (+ x <>)
    integer->char
    string
    string->symbol))

(define (make-template-function n)
  (let loop ((i (- n 1))
             (t (list '->
                      (make-template-arg n))))
    (if (< i 0)
      t
      (loop (- i 1)
            (cons (make-template-arg i)
                  t)))))

(define (template? x env)
  (let ((x (lookup-type x env)))
   (or (template-arg? x)
       (and (list? x)
            (any (cut template? <> env)
                 x)))))

(define (wildcarded? x)
  (or (asterisk? x)
      (and (list? x)
           (any asterisk? x))))

(define (field-sym? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
        (and (> (string-length s) 1)
             (eqv? (string-ref s 0)
                   #\.)
             (not (eqv? (string-ref s 1)
                        #\.))))))

(define (field-name x)
  (string->symbol
    (let ((s (symbol->string x)))
     (substr s 1 (string-length s)))))

(define (struct-fields t)
  (and (type-struct? t)
       (map (lambda (x)
              (and (pair? x)
                   (field-sym? (car x))
                   (field-name (car x))))
            t)))

(define (struct-types t)
  (and (type-struct? t)
       (map (lambda (x)
              (if (and (pair? x)
                       (field-sym? (car x)))
                (cadr x)
                x))
            t)))

(define (untemplate t)
  (cond ((template-arg? t) '*)
        ((list? t)
         (map untemplate t))
        (else t)))

(define (type-name t env)
  (let ((x (lookup-type t env)))
    (cond ((type-parametrised? x)
           (cadr x))
          ((symbol? x) x)
          (else #f))))

(define (lookup-type t env)
  (cond ((null? t)
         'void)
        ((or (special-type-sym? t)
             (asterisk? t)
             (template-arg? t))
         t)
        ((symbol? t)
         (and-let* ((inf (or (get `(type-table ,t) env))))
           (let ((a (and inf (get 'alias inf))))
            (or (and a (lookup-type a env))
                t))))
        ((list? t)
         (let ((t (map (cut lookup-type <> env) t)))
          (and (all id t) t)))
        (else (/ 1 0) (throw 'error "Malformed internal type representation {}"
                     t))))

(define-syntax resolve-types
  (syntax-rules ()
    ((_ (t ...) env body ...)
     (let ((t (lookup-type t env)) ...)
      body ...))))

(define (class-of? t cls env)
  (resolve-types
    (t) env
    (let ((inf (get `(type-table ,(type-name t env)) env)))
     (and inf (in? cls (or (get 'class inf) '()))))))

(define (type-struct t env)
  (let ((inf (get `(type-table ,(base-type t env))
                  env)))
    (if (type-parametrised? t)
      (set-template-args (get 'type inf)
                         (map cons
                              (list-head (get 'args inf)
                                         (length (cddr t)))
                              (cddr t)))
      (or (and inf (get 'type inf))
          t))))

(define (type-simple? t)
  (symbol? t))

(define (type-struct? t)
  (and (pair? t)
       (or (null? t)
           (not (colon? (car t))))
       (not (any arrow? t))
       (not (eqv? (car t) 'or))))

(define (type-union? t)
  (and (pair? t)
       (eqv? (car t) 'or)))

(define (type-function? t)
  (and (list? t)
       (= 1 (count arrow? t))))

(define (type-parametrised? t)
  (and (pair? t)
       (colon? (car t))))

(define (base-type t env)
  (resolve-types
    (t) env
    (cond ((type-parametrised? t)
           (cadr t))
          ((type-simple? t) t)
          (else #f))))

(define (type-args t env)
  (resolve-types
    (t) env
    (and (type-parametrised? t)
         (cddr t))))

(define (function-args t env)
  (resolve-types
    (t) env
    (and (type-function? t)
         (take-while (! arrow?) t))))

(define (function-return-type t env)
  (resolve-types
    (t) env
    (and (type-function? t)
         (unlist (cdr (memv '-> t))))))

(define (type=? a b env)
  (resolve-types
    (a b) env
    (equal? a b)))

(define (type~? a b env)
  (resolve-types
    (a b) env
    (or (asterisk? a)
        (asterisk? b)
        (cond ((and (symbol? a)
                    (symbol? b))
               (eqv? a b))
              ((and (list? a)
                    (list? b))
               (and (= (length a)
                       (length b))
                    (all (cut type~? <> <> env) a b)))
              (else #f)))))

(define (type-complete? t)
  (if (symbol? t)
    (and (not (template-arg? t))
         (not (asterisk? t)))
    (all type-complete? t)))

(define match-template
  (case-lambda
    ((tpl t env)
     (match-template tpl t #f env))
    ((tpl t cast env)
     (define (match tpl t)
       (and (or (type~? tpl t env)
                (and cast
                     (casts? t tpl env)))
            '()))
     (resolve-types
       (tpl t) env
       (cond ((asterisk? t)
              '())
             ((template-arg? t)
              '())
             ((template-arg? tpl)
              (and (not (colon? t))
                   (not (arrow? t))
                   (list (cons tpl t))))
             ((and (symbol? tpl)
                   (symbol? t))
              (match tpl t))
;             ((and (type-parametrised? tpl)
;                   (type-parametrised? t) 
;                   (type-complete? tpl)
;                   (type-complete? t))
;              (match tpl t))
             ((list? tpl)
              (and (list? t)
                   (= (length tpl)
                      (length t))
                   (foldr (cut merge-template-args <> <> cast env)
                          '()
                          (map (cut match-template <> <> cast env)
                               tpl
                               t))))
             (else #f))))))

(define (merge-template-args d1 d2 cast env)
  (define (count-* t)
    (if (symbol? t)
      (if (asterisk? t) 1 0)
      (-> t flatten (count asterisk? <>))))
  (and d1
       d2
       (foldr (lambda (d dict)
                (and dict
                     d
                     (let+ (k . v) d
                       (let ((a (get k dict)))
                        (cond ((not a)
                               (set k v dict))
                              ((type~? a v env)
                               (if (> (count-* a)
                                      (count-* v))
                                 (set k v dict)
                                 dict))
                              (else #f))))))
              d1
              d2)))

(define (set-template-args tpl alist)
  (tree-replace template-arg?
                (disj (cut get <> alist) id)
                tpl))

(define (set-local-template-args tpl alist)
  (let ((args (map car alist)))
   (tree-replace (cut in? <> args)
                 (disj (cut get <> alist) id)
                 tpl)))

(define (declassify-type t)
  (let ((x (and (pair? t)
                (memv '=> t))))
   (if x (cdr x) t)))

(define (class-specializers t)
  (if (and (pair? t)
           (in? '=> t))
    (take-while (! (cut eqv? <> '=>))
                t)
    '()))

(define match-type
  (case-lambda
    ((tpl t env)
     (match-type tpl t #f env))
    ((tpl t cast env)
     (let ((tpl (declassify-type tpl))
           (specs (class-specializers tpl)))
       (resolve-types
         (t tpl) env
         (cond
           ((or (special-type-sym? tpl)
                (special-type-sym? t))
            (and (eqv? tpl t) t))
           ((or (symbol? tpl)
                (symbol? t))
            (cond ((or (template-arg? tpl)
                       (asterisk? tpl))
                   t)
                  ((or (asterisk? t)
                       (template-arg? t))
                   tpl)
                  ((eqv? tpl t)
                   t)
                  (else #f)))
           (else
             (and-let* ((alist (match-template tpl t cast env))
                        (res (set-template-args tpl alist)))
               (if (null? specs)
                 res
                 (and (all (lambda (x)
                             (let+ (class name) x
                               (class-of? (get name alist)
                                          class
                                          env)))
                           specs)
                      res))))))))))

(define (type-completeness t)
  (cond ((symbol? t)
         (if (or (template-arg? t)
                 (asterisk? t))
           0
           1))
        ((null? t) 1)
        ((pair? t)
         (let* ((l (filter (! (disj arrow? colon?))
                           (flatten t)))
                (len (length l)))
           (/ (- len
                 (count (disj template-arg? asterisk?) l))
              len)))))

(define (most-complete-type a b)
  (if (> (type-completeness a)
         (type-completeness b))
    a
    b))

(define (num-expr-type args env)
  (and (all (cut in? <> (get 'numeric-tower env)) args)
       (let* ((nt (get 'numeric-tower env))
              (t (map (lambda (x)
                        (index (cut eqv? (lookup-type x env) <>)
                               nt))
                      args)))
         (and (all id t)
              (list-ref nt (foldr max 0 t))))))

;(define (casts? from to env)
;  (resolve-types
;    (from to) env
;    (let ((inf1 (get `(type-table ,(base-type from env)) env))
;          (inf2 (get `(type-table ,(base-type to env)) env)))
;      (or (and inf1
;               (any (cut type~? to <> env)
;                    (or (get 'casts-to inf1)
;                        '())))
;          (and inf2
;               (any (cut type~? from <> env)
;                    (or (get 'casts-from inf2)
;                        '())))))))

(define (casts-expl? from to env)
  (resolve-types
    (from to) env
    (let ((inf1 (get `(type-table ,(base-type from env)) env))
          (inf2 (get `(type-table ,(base-type to env)) env)))
      (define (set-args% lst t inf)
        (if (type-parametrised? t)
          (set-template-args
            lst
            (map cons
                 (list-head (get 'args inf)
                            (length (cddr t)))
                 (cddr t)))
          lst))
      (or (and inf1
               (any (cut type~? to <> env)
                    (set-args% (append
                                 (or (get 'casts-expl-to inf1)
                                     '())
                                 (or (get 'casts-to inf1)
                                     '()))
                               from
                               inf1)))
          (and inf2
               (any (cut type~? from <> env)
                    (set-args% (append
                                 (or (get 'casts-expl-from inf2)
                                     '())
                                 (or (get 'casts-from inf2)
                                     '()))
                               to
                               inf2)))))))

(define (casts? from to env)
  (resolve-types
    (from to) env
    (let ((inf1 (get `(type-table ,(base-type from env)) env))
          (inf2 (get `(type-table ,(base-type to env)) env)))
      (define (set-args% lst t inf)
        (if (type-parametrised? t)
          (set-template-args
            lst
            (map cons
                 (list-head (get 'args inf)
                            (length (cddr t)))
                 (cddr t)))
          lst))
      (or (and inf1
               (any (cut type~? to <> env)
                    (set-args% (or (get 'casts-to inf1)
                                   '())
                               from
                               inf1)))
          (and inf2
               (any (cut type~? from <> env)
                    (set-args% (or (get 'casts-from inf2)
                                   '())
                               to
                               inf2)))))))

(define (match-args tpl got env)
  (define (match-arg tpl got)
    (or (match-type tpl got env)
        (and (casts? got tpl env)
             tpl)))
  (resolve-types
    (tpl got) env
    (let* ((t2 (function-args got env))
           (tpl (fill-ellipsis tpl t2))
           (t1 (function-args tpl env))
           (args (and (= (length t1)
                         (length t2))
                      (map match-arg t1 t2)))
           (res (match-type (function-return-type tpl env)
                            (function-return-type got env)
                            env)))
      (and (all id args)
           res
           (match-type (filter (! ellipsis?) tpl)
                       (append args (list '-> res))
                       env)))))

(define (fill-ellipsis t need)
  (if (any ellipsis? t)
    (let ((args (take-while (! ellipsis?) t)))
     (append args
             (list-tail need (length args))
             (memv '-> t)))
    t))

(define (find-applicable-functions type type-env funcs verbose)
  (let* ((args (take-while (! arrow?) type))
         (funcs (map (lambda (f)
                       (let+ (t name) f
                         (list (fill-ellipsis t args)
                               name)))
                     funcs)))
    (define (applicable? f-type)
      (define (not-failed x)
        (if (pair? x)
          (all not-failed x)
          x))
      (define (match-cast from to)
        (or (match-type from to type-env)
            (and (casts? from to type-env)
                 to)))
      (let* ((specs (class-specializers f-type))
             (t (declassify-type f-type))
             (l1 (if (any ellipsis? t)
                   (append (list-head type
                                      (index ellipsis? t))
                           (memv '-> type))
                   type))
             (l2 (if (any ellipsis? t)
                   (append (but-last (take-while (! arrow?)
                                                 t))
                           (memv '-> t))
                   t))
             (res (and (= (length l1)
                          (length l2))
                       (map untemplate
                            (map match-cast l1 l2)))))
        (and (not-failed res)
             (match-type f-type res type-env))))
    (define (try-func func p)
      (let+ (t name) func
        (when verbose
          (fmt #t "\t{}: " t))
        (list (or (and (p t)
                       (begin
                         (when verbose
                           (fmt #t "ok\n"))
                         t))
                  (begin
                    (when verbose
                      (fmt #t "fail\n"))
                    #f))
              name)))
    (define (try-funcs p)
      (map (cut try-func <> p) funcs))
    (filter car
            (append
              (try-funcs (cut match-type <> type type-env))
              (try-funcs applicable?)))))

(define (count-eq-types t type type-env)
  (count id (map (cut type=? <> <> type-env)
                 type
                 t)))

(define (type-similarity a b)
  (cond ((or (template-arg? a)
             (template-arg? b))
         3.0)
        ((and (list? a)
              (list? b)
              (= (length a)
                 (length b)))
         (/ (foldr + 0.0 (map type-similarity a b))
            (length a)))
        ((and (symbol? a)
              (symbol? b)
              (eqv? a b))
         10.0)
        (else 0.0)))

(define (find-applicable-function type type-env funcs verbose)
  (when verbose
    (fmt #t "Finding applicable function, need {}\n"
         (type-repr type)))
  (let ((funcs (find-applicable-functions type type-env funcs verbose)))
   (let ((res (and (not (null? funcs))
                   (sort (map (lambda (x)
                                (cons (lookup-type (declassify-type (car x)) type-env)
                                      (cdr x)))
                              funcs)
                         (lambda (a b)
                           (> (type-similarity (car a) type)
                              (type-similarity (car b) type)))))))
     (when verbose
       (if res
         (fmt #t "Result: {}, type: {}\nAll results: {}\n"
              (cadar res)
              (type-repr (caar res))
              (map (comp type-repr car)
                   res))
         (fmt #t "Result: fail\n")))
     (and res (car res)))))

(define (type-repr t)
  (cond ((symbol? t)
         (symbol->string t))
        ((null? t)
         "()")
        ((pair? t)
         (case (car t)
           ((|:|)
            (case (cadr t)
              ((list)
               (format-str
                 "[{}]"
                 (type-repr (caddr t))))
              ((array)
               (format-str
                 "#[{}]"
                 (type-repr (caddr t))))
              (else
                (format-str
                  "(: {} {})"
                  (cadr t)
                  (join-str " " (map type-repr (cddr t)))))))
           (else
             (format-str
               (if (any arrow? t)
                 "({})"
                 "#({})")
               (join-str " " (map type-repr t))))))))

(define (type-data-size t)
  (case t
    ((int) 'quad)
    ((char bool int32) 'long)
    ((int16) 'word)
    ((byte void) 'byte)
    (else 'quad)))

(define (data-size t)
  (case (type-data-size t)
    ((quad) 8)
    ((long) 4)
    ((word) 2)
    ((byte) 1)
    (else (throw 'error "Unknown type ({})" t))))

(define (struct-size t)
  (foldr +
         0
         (map (lambda (x)
                (bytes
                  (type-size
                    (if (and (pair? x)
                             (field-sym? (car x)))
                      (cadr x)
                      x))))
              t)))

(define (struct-offset t idx)
  (struct-size (list-head t idx)))

(define (pad-struct t)
  (let* ((sz (struct-size t))
         (rem (remainder sz 8)))
    (if (= rem 0)
      t
      (append (but-last t)
              (map (lambda (i)
                     (list (makesym ".pad" i)
                           'byte))
                   (range sz))
              (list (last-car t))))))

(define (type-size t)
  (case t
    ((int) 'quad)
    ((byte) 'byte)
    ((char bool float int32) 'long)
    (else 'quad)))

(define (bytes t)
  (case t
    ((quad double) 8)
    ((long float) 4)
    ((word) 2)
    ((byte) 1)))

