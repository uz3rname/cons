;-unit env -uses syntax -uses macroexpand -uses types
(include "inc/macro.scm")
(import (chicken process))

(define (make-env type-env macro-env)
  `((generics)
    (functions)
    (foreign)
    (data)
    (current . #f)
    (macro-env . ,macro-env)
    (single-funcs)
    (hidden)
    (notail)
    (libs)
    (mod-name)
    (lvl . 0)
    (tco)
    (using)
    (export)
    (decls)
    (symbol-table)
    (type-env . ,type-env)))

(define (make-ref ref value . rest)
  (append
    `((ref . ,ref)
      (value . ,value))
    (map* cons (group 2 rest))))

(define (get-function name)
  (do
    ((: functions) <- (get-state))
    (return (get name functions))))

(define (get-subfunctions parent)
  (do
    ((: functions) <- (get-state))
    (return
      (map car
           (filter (comp (cut eqv? <> parent)
                         (cut get 'parent <>)
                         cdr)
                   functions)))))

(define (get-generic name)
  (do
    ((: generics) <- (get-state))
    (return (get name generics))))

(define (current-function)
  (do
    ((: functions current) <- (get-state))
    (return (get current functions))))

(define (toplevel-function)
  (do
    ((: toplevel functions) <- (get-state))
    (return (get toplevel functions))))

(define (get-data name)
  (do
    ((: data) <- (get-state))
    (return (get name data))))

(define (make-function-scope name arg-list type)
  (let ((scope (map (lambda (x)
                      (let+ (i (name type)) x
                        (cons name
                              (make-ref 'arg i 'type type))))
                    (enumerate (zip arg-list
                                    (take-while (! arrow?) type))))))
    (if name
      (set name
           (make-ref 'generic name)
           scope)
      scope)))

(define (true-generic-name name)
  (do
    ((: current module-name toplevel) <- (get-state))
    (return
      (let
       ((name (if (or (not current)
                      (eqv? current toplevel))
                name
                (string->symbol
                  (concat current "-" name)))))
       (if module-name
         (string->symbol
           (concat module-name "." name))
         name)))))

(define (make-function-name g-name prefix)
  (do
    ((: single-funcs) <- (get-state))
    ((: functions) <- (get-generic g-name))
    (if (in? g-name single-funcs)
      (return g-name)
      (if (and (not (eqv? g-name 'lambda))
               (null? functions)
               #f)
        (return g-name)
        (return (string->symbol
                  (concat (if prefix
                            (concat prefix "--")
                            "")
                          g-name
                          "-"
                          (length functions))))))))

(define (ensure-generic name)
  (do
    ((: generics current notail) <- (get-state))
    (g-name <- (true-generic-name name))
    (let ((g (get g-name generics)))
     (if g
       (return g-name)
       (do
         (set-state `(generics ,g-name)
                    `((functions)
                      (parent . ,current)
                      (notail . ,(in? name notail))
                      (templates)))
         (if current
           (def-current name 'generic g-name)
           (return))
         (return g-name))))))

(define (def-current name ref value)
  (do
    ((: current) <- (get-state))
    (set-state `(functions ,current scope ,name)
               (make-ref ref value))))

(define (get-function-by-type g-name type)
  (do
    (gen <- (get-generic g-name))
    ((: type-env) <- (get-state))
    (let ((f (find (comp (cut type=?
                              (function-args type type-env)
                              <>
                              type-env)
                         (cut function-args <> type-env)
                         car)
                   (get 'functions gen))))
      (if f
        (return (cadr f))
        (return #f)))))

(define (find-local-definitions body)
  (map expr-value
       (filter (conj expr?
                     (cut expr-is? <> 'def))
               body)))

(define (add-function g-name f-name type arg-list body tpl-env)
  (do
    (dbgfmt 'env-info
            "Adding function {}:{} ({}), type: {}"
            g-name
            f-name
            arg-list
            (type-repr type))
    ((: type-env current toplevel) <- (get-state))
    (gen <- (get-generic g-name))
    (parent <- (return
                 (if gen
                   (case g-name
                     ((lambda) current)
                     (else (get 'parent gen)))
                   current)))
    (when-m (and (not (eqv? toplevel f-name))
                 (not parent))
      (throw 'env-error "Invalid function parent ({}, {})"
             f-name
             parent))
    (and
      (assert (and arg-list
                   type
                   (= (length arg-list)
                      (length (take-while (! arrow?)
                                          type))))
              'type-error
              "Invalid argument count in {} ({}) {}"
              f-name
              arg-list
              type)
      (do
        (set-state `(functions ,f-name)
                   `((scope . ,(make-function-scope g-name
                                                    arg-list
                                                    type))
                     (arg-list . ,arg-list)
                     (locals . ())
                     (type . ,type)
                     (name . ,f-name)
                     (notail . ,(get 'notail gen))
                     (parent . ,parent)
                     (g-name . ,g-name)
                     (tpl-env . ,tpl-env)
                     (closure-env)
                     (body)))
        (body <- (eval-body f-name body))
        (locals <- (get-subfunctions body))
        (if (or (eqv? f-name toplevel)
                (null? locals))
          (set-state `(functions ,f-name body)
                     (make-expr-at
                       body
                       'sequence
                       #t
                       body)) 
          (do
            (fns <- (map-m (lambda (name)
                             (do
                               (f <- (get-function name))
                               (return (list name (get 'type f)))))
                           locals))
            (set-state `(functions ,f-name body)
                       (make-expr-at
                         body
                         'call
                         #t
                         (cons (make-fn-expr
                                 (expr-file body)
                                 (expr-line body)
                                 (expr-col body)
                                 (map (comp (cut make-expr-at
                                                 body
                                                 'symbol
                                                 <>
                                                 '())
                                            expr-value)
                                      (map car fns))
                                 (make-expr-at
                                   body
                                   'type
                                   (append (map cadr fns)
                                           '(-> *))
                                   '())
                                 body)
                               (map (lambda (name)
                                      (make-expr-at
                                        body
                                        'function
                                        name
                                        '()))
                                    (map car fns)))))))
        (if g-name
          (do
            (f <- (get-function-by-type g-name type))
            (if (or (not f)
                    (eqv? g-name 'lambda))
              (push-state `(generics ,g-name functions)
                          (list type f-name #t))  
              (throw 'def-error
                     "Multiple definition of {} with type {}"
                     g-name
                     (type-repr type))))
          (return))))))

(define (add-template g-name type arg-list body)
  (do
    (dbgfmt 'env-info
            "Adding template {} ({}), type: {}"
            g-name
            arg-list
            (type-repr type))
    ((: importing) <- (get-state))
    (push-state `(generics ,g-name templates)
                `((type . ,(declassify-type type))
                  (specs . ,(class-specializers type))
                  (arg-list . ,arg-list)
                  (imported . ,importing)
                  (body . ,body)))))

(define (eval-body f-name body)
  (do
    ((: current) <- (get-state))
    (set-state 'current f-name)
    (body <- (map-m eval-expr body))
    (set-state 'current current)
    (return (filter (! null?) body))))

(define (eval-def e)
  (let+ (: (name value) sub) e
    (do
      ((: type-env) <- (get-state))
      (let+ (: tag value sub) (car sub)
        (case tag
          ((fn)
           (let+ (arg-list type) value
             (do
               (g-name <- (ensure-generic name))
               (if (or (template? (declassify-type type) type-env)
                       (equal? type '(-> *)))
                 (add-template g-name type arg-list sub)
                 (do
                   (f-name <- (make-function-name g-name #f))
                   (add-function g-name f-name type arg-list sub '())))
               (return '()))))
          (else
            (do
              ((: current toplevel) <- (get-state))
              (let ((const (const? (car (sub-expr e))))
                    (d-name (if (eqv? current toplevel)
                              name
                              (makesym current '_ name))))
                (do
                  (if const
                    (set-state `(data ,d-name)
                               `((type . ,(car const))
                                 (value . ,(cadr const))))
                    (set-state `(data ,d-name)
                               `((type . #f)
                                 (value . 0)
                                 (expr . ,(car (sub-expr e))))))
                  (def-current name 'data d-name)
                  (if const
                    (return '())
                    (return (make-expr
                              (expr-file e)
                              (expr-line e)
                              (expr-col e)
                              'set
                              #t
                              (list (make-expr
                                      (expr-file e)
                                      (expr-line e)
                                      (expr-col e)
                                      'symbol
                                      d-name
                                      '())
                                    (car (sub-expr e)))
                              'init #t))))))))))))

(define (const? e)
  (let+ (: tag value sub) e
    (case tag
      ((int real bool char)
       (list tag value))
      ((cast)
       (or (and (expr-is? (car sub) 'int)
                (type~? value '(|:| ptr *) (init-type-env))
                (list value (expr-value (car sub))))
           (and (expr-is? (car sub) 'list)
                (null? (sub-expr (car sub)))
                (list value 0))
           (and (expr-is? (car sub) 'symbol)
                (eqv? (expr-value (car sub)) 'null-ptr)
                (list value 0))))
      (else #f))))

(define (search-expr p e)
  (let ((sub (or (sub-expr e) '())))
   (append (filter p (cons e sub))
           (mappend (cut search-expr p <>)
                    sub))))

(define (replace-expr p f e)
  (if (p e)
    (replace-expr p f (f e))
    (let+ (: sub) e
      (if sub
        (set 'sub
             (map (cut replace-expr p f <>)
                  sub)
             e)
        e))))

(define (eval-extern e)
  (let+ (name . funcs) (expr-value e)
    (add-extern name funcs)))

(define (add-extern name funcs)
  (do
    ((: type-env current importing) <- (get-state))
    (g-name
      <- (cond
           ((all type-function?
                 (map car funcs))
            (ensure-generic name))
           ((= (length funcs) 1)
            (return name))
           (else (throw 'error "!"))))
    (for-each-m
      (lambda (x)
        (let+ (type name) x
          (let ((tpl (and name (starts-with "tpl__" name)))
                (name (if name
                        (string->symbol name)
                        g-name)))
            (if (type-function? type)
              (do
                (push-state `(generics ,g-name functions)
                            (list type name (not tpl)))
                (set-state `(functions ,name)
                           `((type . ,type)
                             (extern . #t)
                             (imported . ,importing))))
              (do
                (set-state `(data ,name)
                           `((type . ,type)
                             (value . #f)
                             (imported . ,importing)
                             (extern . #t)))
                (def-current g-name 'data name))))))
      funcs)
    (return '())))

(define (extern? x)
  (get 'extern (cdr x)))

(define (eval-decl e)
  (>>=
    (map-m (lambda (x)
             (let+ (decl . args) (if (list? x)
                                   x
                                   (list x))
               (letrec
                 ((push-args
                    (lambda (key)
                      (do
                        (for-each-m (cut push-state key <>)
                                    args)
                        (return '()))))
                  (set-arg
                    (lambda (key val)
                      (do
                        (set-state key val)
                        (return '())))))
                 (case decl
                   ((export)
                    (push-args 'export))
                   ((link-with)
                    (push-args 'libs))
                   ((module)
                    (set-arg 'mod-name (car args)))
                   ((export-all)
                    (set-arg 'export-all #t))
                   ((single)
                    (push-args 'single-funcs))
                   ((hidden)
                    (push-args 'hidden))
                   ((notail)
                    (push-args 'notail))
                   ((import-begin)
                    (set-arg 'importing #t))
                   ((import-end)
                    (set-arg 'importing #f))
                   ((foreign)
                    (push-args 'foreign))
                   ((if-defined)
                    (do
                      ((: macro-env) <- (get-state))
                      (if (in? (car args)
                               (map car macro-env))
                        (return '())
                        (set-arg 'ignore #t))))
                   ((if-eq)
                    (do
                      ((: macro-env) <- (get-state))
                      (if (equal? (get `(,(car args) value) macro-env)
                                  (cadr args))
                        (return '())
                        (set-arg 'ignore #t))))
                   ((def)
                    (do
                      ((: macro-env) <- (get-state))
                      (if (in? (car args)
                               (map car macro-env))
                        (throw 'env-error "Redefinition of {}" (car args))
                        (do
                          (set-state `(macro-env ,(car args))
                                     `((value . ,(cadr args))))
                          (return '())))))
                   ((undef)
                    (do
                      ((: macro-env) <- (get-state))
                      (if (in? (car args)
                               (map car macro-env))
                        (do
                          (set-state `(macro-env)
                                     (del (car args) macro-env)))   
                        (throw 'env-error "{} is undefined" (car args)))))
                   ((else)
                    (do
                      ((: ignore) <- (get-state))
                      (set-arg 'ignore (not ignore))))
                   ((end-if)
                    (set-arg 'ignore #f))
                   (else
                     (throw (expr-error
                              e "Unknown declaration ({})" decl)))))))
           (expr-value e))
    (lambda (e)
      (return (filter (! null?) e)))))

(define (eval-typedef e)
  (let+ (name type alias) (expr-value e)
    (let ((name (if (list? name)
                  (car name)
                  name))
          (args (and (list? name)
                     (cdr name))))
      (do
        ((: type-env importing) <- (get-state))
        (set-state `(type-env type-table ,name)
                   (cond
                     (alias
                       `((alias . ,(lookup-type type type-env))))
                     ((type-struct? type)
                      (let* ((types (struct-types type))
                             (casts (list (untemplate types))))
                        `((type . ,types)
                          (fields . ,(struct-fields type))
                          (casts-from . ,casts)
                          (casts-expl-from . ,casts)
                          (casts-expl-to . ,casts))))
                     ((type-union? type)
                      (let* ((types (struct-types (cdr type))))
                       `((type . ,(cons 'or types))
                         (union . ,(struct-fields (cdr type)))
                         (casts-from . ,types)
                         (casts-expl-from . ,types))))
                     (else
                       (let* ((type (lookup-type type type-env))
                              (casts (append
                                       (list (untemplate type))
                                       (if (and (type-parametrised? type)
                                                (eqv? (cadr type) 'list))
                                         (list 'null)
                                         '()))))
                         `((type . ,type)
                           (casts-from . ,casts)
                           (casts-expl-from . ,casts)
                           (casts-expl-to . ,casts))))))
        (if args
          (set-state `(type-env type-table ,name args)
                     args)
          (return))
        (set-state `(type-env type-table ,name imported)
                   importing)
        (return '())))))

(define (eval-sequence e)
  (do
    (sub <- (map-m eval-expr (sub-expr e)))
    (let ((sub (filter (! null?) sub)))
     (if (null? sub)
       (return '())
       (>>=
         (map-m eval-expr sub)
         (lambda (sub)
           (return (update-expr
                     e
                     'sub sub))))))))

(define eval-expr-dispatch
  (list 'def eval-def
        'extern eval-extern
        'decl eval-decl
        'typedef eval-typedef
        'sequence eval-sequence
        'let (cut return <>)
        'fn (cut return <>)))

(define (eval-expr e)
  (do
    ((: ignore) <- (get-state))
    (if (and ignore
             (not (and (expr? e)
                       (expr-is? e 'decl)
                       (in? (caar (expr-value e))
                            '(else end-if)))))
      (return '())
      (let ((x (memv (expr-tag e)
                     eval-expr-dispatch)))
        (if x
          ((cadr x) e)
          (>>=
            (map-m eval-expr (sub-expr e))
            (lambda (sub)
              (if (any null? sub)
                (throw 'error
                       "Invalid expression ({})"
                       (expr-repr e))
                (return (update-expr
                          e
                          'sub sub))))))))))

(define (eval-include x dirs)
  (and (assert (and (= (length x) 2)
                    (expr? (cadr x))
                    (expr-is? (cadr x) 'string))
               (syntax-error x "Invalid include form"))
       (let* ((name (expr-value (cadr x)))
              (file (or (find-file name dirs)
                        (throw 'error
                               "Couldn't find {}"
                               name)))
              (e (parse-file file dirs)))
         (do
           (dbgfmt 'verbose "Including {}" name)
           (eval-includes e dirs)))))

(define (eval-use name dirs)
  (let ((mod (or (find-file (concat name ".a")
                            dirs)
                 (throw 'error
                        "Couldn't find module {}"
                        name))))
    (do
      ((: using) <- (get-state))
      (if (in? name using)
        (return '())
        (do
          (dbgfmt 'verbose "Importing {}" mod)
          (push-state 'using name)
          (let ((e (call-with-input-pipe
                     (fmt #f "{} p {} .header" ar-program mod)
                     (comp (cut parse-string <> ".header" dirs)
                         read-contents))))
            (do
              (set-state 'importing #t)
              (e <- (>>=
                      (eval-includes e dirs)
                      eval-macro-defs))
              (set-state 'importing #f)
              (return e))))))))

(define (eval-includes e dirs)
  (do
    ((: lib-dir) <- (get-state))
    (e <- (map-m (lambda (x)
                   (cond ((expr-list-is? x 'include)
                          (eval-include x dirs))
                         ((expr-list-is? x 'use)
                          (>>=
                            (map-m (cut eval-use <> lib-dir)
                                   (map expr-value (cdr x)))
                            (lambda (xs)
                              (return (foldl append '() xs)))))
                         (else (return (list x)))))
                 e))
    (return (foldl append '() e))))

(define (eval-unit e options)
  (let+ (: inc lib-dir toplevel mod-name) options
    (let ((toplevel (or toplevel 'toplevel)))
      (let+ (: value sub) e
        (let-values (_ env)
          ((do
             (set-state 'lib-dir lib-dir)
             (set-state 'inc-dir inc)
             (set-state 'filename value)
             (set-state 'options options)
             (set-state 'toplevel toplevel)
             (for-each-m
               (lambda (x)
                 (let* ((i (string-search "=" x))
                        (name (makesym
                                (if i
                                  (substr x 0 i)
                                  x)))
                        (value (if i
                                 `((value . ,(makesym (substr x (+ i 1)))))
                                 '())))
                   (set-state `(macro-env ,name)
                              value)))
               (get 'def options))
             (ensure-generic 'lambda)
             (g-name <- (ensure-generic toplevel))
             (push-state 'export toplevel)
             (sub <- (>>=
                       (eval-includes sub inc)
                       macro-expand))
             (add-function g-name
                           toplevel
                           '(-> *)
                           '()
                           sub
                           '()) 
             ((: libs) <- (get-state))
             (set-state `(data __depends)
                        `((type . byte)
                          (value . ,(map char->integer
                                         (string->list
                                           (join-str
                                             ","
                                             (unique libs))))))))
           (make-env (init-type-env)
                     (init-macro-env)))
          env)))))
