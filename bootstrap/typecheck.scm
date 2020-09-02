;-unit typecheck -uses env -uses types
(include "inc/macro.scm")

(define (type-error e msg . rest)
  (apply expr-error
         'type-error
         e
         msg
         rest))

(define (type-mismatch e need got)
  (type-error e
              "Expected {}, got {}"
              (type-repr need)
              (type-repr got)))

(define (type-assert e need)
  (define (type-assert% e need)
    (do
      ((: type-env) <- (get-state))
      (let ((type (expr-type e)))
        (cond ((or (eqv? need '*)
                   (match-type type need type-env))
               (return e))
              ((type=? type need type-env)
               (return e))
              ((casts? type need type-env)
               (cast e need))
              (else (throw (type-mismatch e need type)))))))
  (do
    ((: type-env) <- (get-state))
    (e <- (type-check-expr e))
    (let+ (: tag type value) e
      (if (template? type type-env)
        (do
          (ref-t <- (lookup value))
          (let+ (: ref value lvl) ref-t
            (if (and (eqv? ref 'arg)
                     (= lvl 0))
              (do
                ((: current) <- (get-state))
                ((: (f-type type)) <- (current-function))
                (set-state `(functions ,current type)
                           (match-type f-type
                                       (replace-ref (replace
                                                      (! arrow?)
                                                      (const '*)
                                                      f-type)
                                                    value
                                                    need)
                                       type-env))
                (got <- (get-arg-type value #f))
                (type-assert% (update-expr
                                e
                                'type got)
                              need))
              (throw (type-error e "Invalid reference")))))
        (type-assert% e need)))))

(define (type-assert-same e)
  (do
    (e <- (type-check-expr-list e))
    ((: type-env) <- (get-state))
    (let*
     ((types (map expr-type e))
      (type
        (or (num-expr-type types type-env)
            (find (lambda (t)
                    (all id
                         (map (disj (cut type=? <> t type-env)
                                    (cut casts? <> t type-env))
                              types)))
                  types))))
     (if type
       (do
         (e <- (map-m (lambda (e)
                        (if (type=? (expr-type e) type type-env)
                          (return e) 
                          (cast e type)))
                      e))
         (return (list type e)))
       (throw (type-error
                e
                "Can't coerce expressions to same type"))))))

(define (expr-type e)
  (or (get 'type e)
      (throw (type-error e "Expression is untyped"))))

(define (type-check-functions names)
  (do
    (for-each-m type-check-function
                names)
    ((: functions) <- (get-state))
    (for-each-m closurise-recursive
                (map car
                     (filter (! extern?)
                             functions)))))

(define (type-check-unit e)
  (let+ (: functions toplevel) e
    (let-values (_ env) ((type-check-functions
                           (cons toplevel
                                 (remove 'toplevel
                                         (map car (filter (! extern?)
                                                          functions)))))
                         e)
      env)))

(define (type-check-function name)
  (do
    ((: type-checked) <- (get-function name))
    (unless-m type-checked
      ((: current type-env lvl toplevel) <- (get-state))
      ((: body type parent arg-list scope g-name)
       <- (get-function name))
      (dbgfmt 'type-info
              "Type checking function {}, type: {}"
              name
              (type-repr type))
      ((: (top-scope scope)) <- (get-function toplevel))
      (set-state 'current name)
      (set-state 'lvl 0)
      ((: options) <- (get-state))
      (when-m (in? (symbol->string g-name)
                   (get 'dump-funcs options)
                   string=?)
        (set-state '(options type-info) #t))
      (body <- (type-check-expr body))
      ((: (f-type type)) <- (get-function name))
      (let ((need (function-return-type f-type type-env))
            (got (expr-type body)))
        (cond
          ((or (eqv? need '*)
               (match-type got need type-env))
           (do
             (set-state `(functions ,name body) body)
             (set-state `(functions ,name type)
                        (append (function-args f-type
                                               type-env)
                                (list '-> got)))))
          ((template? need type-env)
           (let ((type (match-type f-type
                                   (append (repeat '*
                                                   (- (length f-type) 2))
                                           (list '-> got))
                                   type-env)))
             (do
               (set-state `(functions ,name body) body)  
               (set-state `(functions ,name type) type)
               ((: functions) <- (get-generic g-name))
               (set-state `(generics ,name functions)
                          (replace (comp (cut eqv? <> name) cadr)
                                   (comp (cut cons type <>) cdr)
                                   functions)))))
          (else
            (do
              (body <- (type-assert body need))
              (set-state `(functions ,name body) body)))))
      ((: type) <- (get-function name))
      (set-state '(options type-info)
                 (get 'type-info options))
      (set-state `(functions ,name type-checked) #t)
      (set-state 'current current)
      (set-state 'lvl lvl)
      (subs <- (get-subfunctions name))
      (for-each-m type-check-function subs))))

(define (lvl-push)
  (do
    ((: lvl) <- (get-state))
    (set-state 'lvl (+ lvl 1))))

(define (lvl-pop)
  (do
    ((: lvl) <- (get-state))
    (set-state 'lvl (- lvl 1))))

(define (cast e to)
  (do
    ((: type-env) <- (get-state))
    (let* ((type (expr-type e))
           (s (type-struct to type-env))
           (st (and (type-union? s)
                    (index (cut type=? <> type type-env)
                           (cdr s))))) 
      (return
        (cond
          (st
            (make-expr-at
              e
              'struct
              #t
              (list (make-expr-at
                      e
                      'int st '()
                      'type 'int)
                    e)
              'type to))  
          ((and (type~? to '(|:| array *) type-env)
                (type=? type 'null type-env))
           (update-expr
             e
             'type to))
          ((and (expr-is? e 'int)
                (eqv? to 'real))
           (make-expr-at
             e
             'real
             (* 1.0 (expr-value e))
             '()
             'type 'real))
          (else
            (make-expr-at
              e
              'cast
              to
              (list e)
              'type to)))))))

(define (type-check-expr-list e)
  (map-m type-check-expr e))

(define (type-check-expr e)
  (let+ (: tag type) e
    (if type
      (return e)
      (case tag
        ((int char real string bool void)
         (return (update-expr
                   e 'type tag)))
        ((num-expr)
         (type-check-num-expr e))
        ((sequence)
         (type-check-sequence e))
        ((symbol)
         (type-check-symbol e))
        ((call)
         (type-check-call e))
        ((if)
         (type-check-if e))
        ((cmp-expr)
         (type-check-cmp e))
        ((bool-expr)
         (type-check-bool e))
        ((cast)
         (type-check-cast e))
        ((struct)
         (type-check-struct e))
        ((array list c-array)
         (type-check-elem-sequence e))
        ((fn)
         (type-check-lambda e))
        ((field)
         (type-check-field e))
        ((set)
         (type-check-set e))
        ((mov)
         (type-check-mov e))
        ((deref)
         (type-check-deref e))
        ((offset)
         (type-check-offset e))
        ((is)
         (type-check-is e))
        ((quoted)
         (type-check-quoted e))
        (else (throw (type-error
                       e "Unknown expression tag {}" tag)))))))

(define (coerce-numbers lst)
  (define (is-real? x)
    (in? x '(real float)))
  (do
    ((: type-env) <- (get-state))
    (let* ((res-type (or
                       (num-expr-type (map expr-type lst)
                                      type-env)
                       (throw (type-error
                                lst 
                                "Couldn't determine expression type ({})"
                                (join-str ", "
                                          (map (comp type-repr expr-type)
                                               lst))))))
         (is-real (is-real? res-type)))
    (>>=
      (map-m (lambda (e)
               (let+ (: type) e
                 (if (or (eqv? type res-type)
                         (and (not is-real)
                              (not (is-real? type))))
                   (return e)
                   (cast e res-type))))
             lst)
      (lambda (l)
        (return (list l res-type)))))))

(define (type-check-num-expr e)
  (let+ (: sub) e
    (do
      (lvl-push)
      ((: type-env) <- (get-state))
      (sub <- (type-check-expr-list sub))
      (lvl-pop)
      (do
        ((sub res-type) <- (coerce-numbers sub))
        (return
          (update-expr
            e
            'type res-type
            'sub sub))))))

(define (type-check-sequence e)
  (let+ (: sub) e
    (if (null? sub)
      (return (update-expr
                e 'type 'void))
      (do
        (lvl-push)
        (sub-e <- (type-check-expr-list (but-last sub)))
        (lvl-pop)
        (last <- (type-check-expr (last-car sub)))
        (return
          (update-expr
            e
            'type (expr-type last)
            'sub (append sub-e (list last))))))))

(define (lookup name . args)
  (define (lookup-in lvl f-name)
    (if (not f-name)
      (return #f)
      (do
        ((: scope parent) <- (get-function f-name))
        (let ((ref (get name scope)))
         (if ref
           (return (set 'lvl lvl ref))
           (lookup-in (+ lvl 1) parent))))))
  (let ((in (opt-arg args 0 #f)))
   (if in
     (lookup-in 0 in)
     (do
       ((: current) <- (get-state))
       (lookup-in 0 current)))))

(define (type-check-symbol e)
  (let+ (: (name value)) e
    (do
      (ref-t <- (lookup name))
      (if (not ref-t)
        (throw (type-error
                 e "Undefined symbol {}" name))
        (let+ (: ref value type lvl) ref-t
          (case ref
            ((arg closure)
             (do
               (if (> lvl 0)
                 (do
                   ((: current) <- (get-state))
                   (ensure-closure-ref current name type lvl)
                   (type-check-symbol e))
                 (return
                   (update-expr
                     e
                     'type type
                     'ref ref-t)))))
            ((data)
             (do
               ((: type) <- (get-data value))
               (return
                 (update-expr
                   e
                   'type type
                   'ref (make-ref 'symbol-value
                                  value)))))
            (else
              (throw 'error
                     "Invalid reference {} ({} {})"
                     name
                     ref
                     ref-t))))))))

(define (ensure-closure-ref f-name name type lvl)
  (do
    ((: scope parent) <- (get-function f-name))
    (unless-m (find (comp (cut eqv? name <>)
                          car)
                    scope)
      (do
        (dbgfmt 'type-info
                "Closure reference: {} in {}"
                name
                f-name)
        (push-state `(functions ,f-name closure-env)
                    name)
        ((: closure-env) <- (get-function f-name))
        (push-state `(functions ,f-name scope)
                    (cons name
                          `((ref . closure)
                            (type . ,type)
                            (lvl . ,lvl)
                            (value . ,(index (cut eqv? name <>)
                                             closure-env)))))
        ((: toplevel) <- (get-state))
        (unless-m (eqv? parent toplevel)
          ((: scope) <- (get-function parent))
          (unless-m (get name scope)
            (ensure-closure-ref parent name type (- lvl 1))))))))

(define (scalar? t)
  (in? t '(bool int char real ptr)))

(define (get-arg-type n f-name)
  (do
    ((: type) <- (if f-name
                   (get-function f-name)
                   (current-function)))
    ((: type-env) <- (get-state))
    (return
      (list-ref (function-args type type-env)
                n))))

(define (dispatch-call e need)
  (define (approx-args args need)
    (do
      (ap-args <- (map-m (cut approx-type <> <>)
                         args
                         need))
      (unless-m (all id ap-args)
        (throw (type-error
                 e
                 "Couldn't approximate argument types (given: {})"
                 (type-repr need))))
      (dbgfmt 'type-info
              "Approx args: {}"
              (type-repr ap-args))
      (return ap-args)))
  (define (approx-op op need)
    (do
      (ap-t <- (approx-type op need))
      (unless-m ap-t
        (throw (type-error
                 e
                 "Couldn't approximate operator type (given: {})"
                 (type-repr need))))
      (dbgfmt 'type-info
              "Approx func: {}"
              (type-repr ap-t))
      (return ap-t)))
  (let+ (op . args) (sub-expr e)
    (do
      (dbgfmt 'type-info
              "Dispatching call: {}, expecting {}"
              (expr-repr e)
              (type-repr need))
      ((: type-env) <- (get-state))
      (lvl-push)
      ((ap-args ap-t)
       <- (let ((t-ret (function-return-type need type-env))
                (t-args (function-args need type-env)))
            (define (match-approx ap-t ap-args ret)
              (let ((ap-args (untemplate
                               (match-args ap-t
                                           (append ap-args
                                                   (list '-> ret))
                                           type-env))))
                (do
                  (dbgfmt 'type-info
                          "Approx args*: {}"
                          (type-repr ap-args))
                  (return (list ap-args ap-t)))))
            (if (and (type-function? t-ret)
                     (not (all asterisk?
                               (function-args t-ret type-env)))
                     (or (null? t-args)
                         (all asterisk? t-args)))
              (do
                (ap-t <- (approx-op op need))
                (ap-args <- (approx-args args
                                         (match-type ap-t
                                                     (append
                                                       t-args
                                                       (list '-> t-ret))
                                                     type-env)))
                (match-approx ap-t ap-args t-ret))
              (do
                (ap-args <- (approx-args args t-args))
                (ap-t <- (approx-op op
                                    (append ap-args
                                            (list '-> t-ret))))
                (match-approx ap-t ap-args '*)))))
      (if (not ap-t)
        (throw (type-error e "Couldn't approximate function (args: {})"
                           (join-str ", "
                                     (map type-repr ap-args))))
        (do
          ((args f-type) <- (dispatch-args args
                                           ap-args
                                           ap-t))
          (dbgfmt 'type-info
                  "Arguments dispatching result: ({}), type: {}"
                  (join-str " " (map expr-repr args))
                  (type-repr f-type))
          (let ((arg-types (map expr-type args)))
            (do
              (op <- (dispatch-expr op f-type))
              (lvl-pop)
              (tail <- (tail-call? op))
              (when-m tail
                (dbgfmt 'type-info
                        "Tail call detected: {}"
                        (expr-repr e)))
              (func <- (if (expr-is? op 'function)
                         (get-function (expr-value op))
                         (return #f)))
              ((: foreign) <- (get-state))
              (return (update-expr
                        e
                        'type (function-return-type
                                (expr-type op)
                                type-env)
                        'sub (cons (update-expr
                                     op
                                     'tail tail)
                                   (if (and func
                                            (in? (expr-value op)
                                                 foreign))
                                     (map (cut set 'ptr #t <>)
                                          args)
                                     args))
                        'ellipsis (and func
                                       (any ellipsis?
                                            (get 'type func)))
                        'value tail)))))))))

(define (type-check-call e)
  (dispatch-call e
                 (append (repeat '*
                                 (length (cdr (sub-expr e))))
                         '(-> *))))

(define (tail-call? op)
  (define (find-parent cur name lvl)
    (if (eqv? cur name)
      (do
        ((: tco) <- (get-state))
        (if (not (in? cur tco))
          (push-state 'tco cur)
          (return))
        (return lvl))
      (do
        ((: parent) <- (get-function cur))
        (if parent
          (find-parent parent name (+ lvl 1))
          (return #f)))))
  (do
    ((: lvl current) <- (get-state))
    (if (and (zero? lvl)
             (expr-is? op 'function))
      (do
        ((: notail) <- (get-function (expr-value op)))
        (if notail
          (return #f)
          (find-parent current (expr-value op) 0)))
      (return #f))))

(define (approx-type e ap-type)
  (define (approx-expr-type e)
    (do
      (e <- (type-check-expr e))
      (return (expr-type e))))
  (let+ (: tag value sub type) e
    (if type
      (return type)
      (do
        (dbgfmt 'type-info
                "Approximating {}, expecting: {}"
                (expr-repr e)
                (type-repr ap-type))
        ((: type-env) <- (get-state))
        (ap-type <- (return (lookup-type ap-type type-env)))
        (case tag
          ((fn)
           (let+ (_ f-type) (expr-value e)
             (let ((m (match-type f-type ap-type type-env)))
              (if m
                (return m)
                (throw (type-mismatch e ap-type f-type))))))
          ((symbol)
           (do
             (ref-t <- (lookup value))
             (if (not ref-t)
               (throw (type-error
                        e "Undefined symbol {}" value))
               (let+ (: ref value) ref-t
                 (case ref
                   ((generic)
                    (case ap-type
                      ((*) (return '*))
                      (else (approx-generic value ap-type))))
                   (else (approx-expr-type e)))))))
          ((list array)
           (if (null? sub)
             (return `(|:| ,tag *))
             (approx-expr-type e)))
          (else (approx-expr-type e)))))))

(define (approx-generic name type)
  (do
    (dbgfmt 'type-info
            "Approximating generic: {}, expecting {}"
            name
            (type-repr type))
    ((: functions templates) <- (get-generic name))
    ((: type-env options) <- (get-state))
    (let ((func (find-applicable-function type
                                          type-env
                                          (append (filter caddr functions)
                                                  (map (lambda (t)
                                                         (let+ (: specs type) t
                                                           (list (if (null? specs)
                                                                   type  
                                                                   (append specs
                                                                           (cons '=> type)))
                                                                 #f)))
                                                       templates))
                                          (get 'type-info options))))
      (return (and func (car func))))))

(define (dispatch-args args arg-types f-type)
  (do
    (dbgfmt 'type-info
            "Dispatching arguments: {}, approx: {}, function: {}"
            (map expr-repr args)
            (type-repr arg-types)
            (type-repr f-type))
    ((: type-env) <- (get-state))
    (let ((lst (sort (enumerate (zip arg-types args))
                     (lambda (a b)
                       (< (count asterisk?
                                 (flatten (mklist (car (cadr a)))))
                          (count asterisk?
                                 (flatten (mklist (car (cadr b))))))))))
      (define (loop res args arg-types)
        (if (null? args)
          (return (list (reverse res)
                        arg-types))
          (let+ (head . tail) args
            (let ((i (car head))
                  (e (cadr (cadr head))))
              (do
                (e <- (dispatch-expr e (list-ref arg-types i)))
                (loop (cons (list i e) res)
                      tail
                      (untemplate
                        (match-type f-type
                                    (replace-ref arg-types
                                                 i
                                                 (expr-type e))
                                    type-env))))))))
      (do
        ((args type) <- (loop '() lst arg-types))
        (return (list (map cadr
                           (sort args
                                 (lambda (a b)
                                   (< (car a) (car b)))))
                      type))))))

(define (dispatch-expr e type)
  (do
    (dbgfmt 'type-info
            "Dispatching expr: {}, type: {}"
            (expr-repr e)
            (type-repr type))
    ((: type-env) <- (get-state))
    (let+ (: tag value (e-type type)) e
      (if e-type
        (if (match-type e-type type type-env)
          (return e)
          (throw (type-mismatch e type e-type)))
        (case tag
          ((symbol)
           (do
             (ref-t <- (lookup value))
             (if (not ref-t)
               (throw (type-error
                        e "Undefined symbol {}" value))
               (let+ (: ref value lvl) ref-t
                 (case ref
                   ((generic)
                    (do
                      (res <- (dispatch-generic e value type))
                      (dbgfmt 'type-info
                              "Generic dispatch result ({}): {} ({})"
                              value
                              (expr-value res)
                              (type-repr (expr-type res)))
                      (if res
                        (return res)
                        (throw (type-error
                                 e "Couldn't dispatch generic {}"
                                 (expr-repr e))))))
                   (else (type-assert e type)))))))
          ((fn)
           (dispatch-lambda e type))
          ((array)
           (do
             (e <- (type-assert e type))
             (let+ (: (e-type type)) e
               (case e-type
                 ((null)
                  (let ((st (car (type-args type type-env))))
                   (update-expr
                     e
                     'type type)))
                 (else (return e))))))
          ((call)
           (do
             (e <- (dispatch-call
                     e
                     (append (repeat '* (length (cdr (sub-expr e))))
                             (list '-> type))))
             (type-assert e type)))
          (else (type-assert e type)))))))

(define (dispatch-generic e name type)
  (do
    (dbgfmt 'type-info
            "Dispatching generic {}, type: {}"
            name
            (type-repr type))
    (gen <- (get-generic name))
    ((: options type-env (all-funcs functions)) <- (get-state))
    (let+ (: functions templates) gen
      (let* ((t-args (function-args type type-env))
             (func (find-applicable-function type
                                             type-env
                                             (filter caddr functions)
                                             (get 'type-info options))))
        (if func
          (let+ (f-type f-name) func
            (do
              (f <- (get-function f-name))
              (return (make-expr-at
                        e
                        'function
                        f-name
                        '()
                        'type (if (and (get 'extern f)
                                       (template? f-type type-env))
                                (match-type f-type type type-env)
                                f-type)))))
          (let* ((matches (filter (cut get 'match <>)
                                  (map (lambda (tpl)
                                         (set 'match
                                              (match-type (get 'type tpl)
                                                          type
                                                          type-env)
                                              tpl))
                                       templates)))
                 (match (and (not (null? matches))
                             (car (sort matches
                                        (lambda (a b)
                                          (> (type-similarity (get 'type a)
                                                              type)
                                             (type-similarity (get 'type b)
                                                              type))))))))
            (if match
              (let+ (: match arg-list body (tpl type) imported) match
                (do
                  (f-name <- (get-function-by-type name match))
                  ((f-type f-name)
                   <- (if f-name
                        (do
                          ((: type) <- (get-function f-name))
                          (return (list type f-name)))
                        (type-check-new-function
                          name match arg-list body
                          (map (lambda (p)
                                 (cons (makesym '< (car p) '>)
                                       (cdr p)))
                               (match-template tpl match type-env)))))
                  (if imported
                    (set-state `(functions ,f-name imported) #t)
                    (return))
                  (return (make-expr-at
                            e
                            'function
                            f-name
                            '()
                            'type f-type))))
              (return #f))))))))

(define (dispatch-lambda e type)
  (let+ (: value sub) e
    (let+ (arg-list f-type) value
      (do
        ((: type-env) <- (get-state))
        (let ((match (match-type f-type type type-env)))
         (if (not match)
           (throw (type-mismatch e type f-type))
           (do
             ((f-type f-name)
              <- (type-check-new-function
                   'lambda
                   match
                   arg-list
                   sub
                   #f))
             (make-lambda-expr e f-name f-type))))))))

(define (type-check-new-function g-name type arg-list body tpl)
  (do
    (f-name <- (make-function-name g-name (and tpl 'tpl)))
    (add-function g-name f-name type arg-list body tpl)
    (type-check-function f-name)
    ((: type g-name) <- (get-function f-name))
    ((: functions) <- (get-generic g-name))
    (set-state `(generics ,g-name functions)
               (append (filter (! (comp (cut eqv? <> f-name)
                                        cadr))
                               functions)
                       (list (list type f-name #f))))
    (return (list type f-name))))

(define (closurise-recursive f-name)
  (do
    (subs <- (get-subfunctions f-name))
    (for-each-m closurise-recursive subs)
    (closurise-local-functions f-name subs #f 0)))

(define (closure-replacer scope functions)
  (lambda (e)
    (let+ (: value) e
      (make-expr-at
        e
        'closure
        value
        (map (lambda (name)
               (let ((ref (get name scope))
                     (arg-types (map (comp (cut get 'type <>)
                                           cdr)
                                     scope))) 
                 (make-expr-at
                   e
                   'symbol
                   #f
                   '()
                   'type (get 'type ref)
                   'ref (-> ref
                          (set 'arg-types
                               arg-types
                               <>)))))
             (get `(,value closure-env)
                  functions))
        'type (get `(,value type)
                   functions)))))

(define closurise-local-functions
  (case-lambda
    ((f-name)
     (>>=
       (get-subfunctions f-name)
       (lambda (subs)
         (closurise-local-functions f-name subs #f 0))))
    ((f-name subs scope lvl)
     (do
       ((: functions) <- (get-state))
       (func <- (get-function f-name))
       (let ((body (get 'body func)))
         (define (subfunction-ref? e)
           (and (expr? e)
                (expr-is? e 'function)
                (in? (expr-value e)
                     subs)
                (not (get 'tail e))
                (not
                  (null?
                    (get `(,(expr-value e) closure-env)
                         functions)))))
         (do
           (for-each-m
             (lambda (name)
               (do
                 ((: closure-env) <- (get-function name))
                 (for-each-m
                   (lambda (name)
                     (do
                       (ref <- (lookup name f-name))
                       (ensure-closure-ref f-name name (get 'type ref) (get 'lvl ref))))
                   closure-env)))
             (map expr-value
                  (search-expr subfunction-ref?
                               body)))
           ((: scope) <- (get-function f-name))
           (set-state `(functions ,f-name body)
                      (replace-expr subfunction-ref?
                                    (closure-replacer scope functions)
                                    body))
           (children <- (get-subfunctions f-name))
           (for-each-m (cut closurise-local-functions <> subs #f 0)
                       children)))))))

(define (type-check-if e)
  (let+ (cnd . branches) (sub-expr e)
    (do
      (lvl-push)
      (cnd <- (type-assert cnd 'bool))
      (lvl-pop)
      ((type branches) <- (type-assert-same branches))
      ((: type-env) <- (get-state))
      (return
        (update-expr
          e
          'type type
          'sub (cons cnd branches))))))

(define (type-check-cmp e)
  (let+ (: file line col sub value) e
    (do
      ((: type-env) <- (get-state))
      (lvl-push)
      (sub <- (type-check-expr-list sub))
      (lvl-pop)
      ((sub type)
       <- (cond ((all (comp (disj (cut type=?
                                       <>
                                       (expr-type (car sub))
                                       type-env)
                                  (cut casts?
                                       <>
                                       (expr-type (car sub))
                                       type-env))
                            expr-type)
                      (cdr sub))
                 (return (list sub (expr-type (car sub)))))
                ((all (comp (cut in? <> (get 'numeric-tower type-env))
                            expr-type)
                      sub)
                 (coerce-numbers sub))
                (else (throw (type-error e "Invalid comparision")))))
      (if (or (in? value '(eq neq))
              (type=? type 'bool type-env)
              (type=? type 'char type-env)
              (in? type (get 'numeric-tower type-env)))
        (return
          (update-expr
            e
            'type 'bool
            'sub sub
            'arg-type type
            'value (case value
                     ((eq) '=)
                     ((neq) '/=)
                     (else value))))
        (type-check-call
          (make-expr file
                     line
                     col
                     'call
                     #t
                     (cons (make-expr file
                                      line
                                      col
                                      'symbol
                                      value
                                      '())
                           (sub-expr e))))))))

(define (type-check-bool e)
  (let+ (: value sub) e
    (and (assert (case value
                   ((not) (= (length sub) 1))
                   (else #t))
                 (type-error e "Invalid argument count"))
         (do
           (lvl-push)
           (sub <- (map-m (cut type-assert <> 'bool)
                          sub))
           (lvl-pop)
           (return (update-expr
                     e
                     'type 'bool
                     'sub sub))))))

(define (type-check-cast e)
  (let+ (: (to value) sub) e
    (do
      (sub <- (type-check-expr (car sub)))
      ((: type-env) <- (get-state))
      (to <- (update-type to))
      (let ((from (expr-type sub)))
       (cond ((casts-expl? from to type-env)
              (cast sub to))
             ((type=? from to type-env)
              (return sub))
             ((or (and (type-struct? (type-struct from type-env))
                       (type=? to '(|:| ptr void) type-env))
                  (and (type-struct? (type-struct to type-env))
                       (type=? from '(|:| ptr void) type-env)))
              (return (update-expr
                        sub
                        'type to)))
             (else
               (throw (type-error
                        e "Can't cast from {} to {}"
                        (type-repr from)
                        (type-repr to)))))))))

(define (type-check-struct e)
  (do
    (lvl-push)
    (sub <- (map-m type-check-expr (sub-expr e)))
    (lvl-pop)
    (return
      (update-expr
        e
        'sub sub
        'type (map expr-type sub)))))

(define (type-check-elem-sequence e)
  (let+ (: tag file line col sub) e
    (if (null? sub)
      (case tag
        ((list c-array)
         (return (make-expr file line col
                            'null #t '()
                            'type 'null
                            'element-type #f)))
        ((array)
         (return (update-expr
                   e
                   'type 'null
                   'element-type #f))))
      (do
        (lvl-push)
        ((type sub) <- (type-assert-same sub))
        (lvl-pop)
        (return
          (update-expr
            e
            'sub sub
            'type (case tag
                    ((c-array)
                     `(|:| ptr ,type))
                    (else `(|:| ,(expr-tag e) ,type)))
            'element-type type))))))

(define (update-type t)
  (do
    ((: tpl-env) <- (current-function))
    (if tpl-env
      (return
        (if (pair? t)
          (set-local-template-args t tpl-env)
          (car (set-local-template-args
                 (list t)
                 tpl-env))))
      (return t))))

(define (type-check-lambda e)
  (let+ (: tag line col value sub name) e
    (do
      (type <- (update-type (cadr value)))
      (g-name <- (if name
                   (ensure-generic name)
                   (return 'lambda)))
      ((f-type f-name)
       <- (type-check-new-function
            g-name
            type
            (car value)
            sub
            #f))
      (make-lambda-expr e f-name f-type))))

(define (make-lambda-expr e f-name f-type)
  (return (make-expr-at
            e
            'function
            f-name
            '()
            'type f-type
            'lambda #t)))

(define (type-check-field e)
  (let+ (: (field value) sub file line col) e
    (do
      (lvl-push)
      (struct <- (type-check-expr (car sub)))
      (lvl-pop)
      ((: type-env) <- (get-state))
      (cond
        ((symbol? field)
         (let ((type (expr-type struct)))
          (do
            ((: type-env) <- (get-state))
            (let* ((inf (get `(type-table ,(base-type type type-env))
                             type-env))
                   (f-list (and inf (get 'fields inf)))
                   (u-list (and inf (get 'union inf))))
              (cond
                (u-list
                  (let ((s (type-struct type type-env)))
                   (if (in? field u-list)
                     (let ((idx (index (cut eqv? <> field)
                                       u-list)))
                       (do
                         (e-cmp <- (type-check-expr
                                     (make-expr file line col
                                                'is field (list struct))))
                         (e-err <- (type-check-expr
                                     (make-expr-from '(die "Invalid union tag")
                                                     file
                                                     line
                                                     col)))
                         (return
                           (make-expr file line col
                                      'if #t
                                      (list e-cmp
                                            (make-expr file line col
                                                       'ref 1
                                                       (list struct)
                                                       'struct-type '(int (: ptr void))
                                                       'type (list-ref
                                                               (cdr s)
                                                               idx))
                                            e-err)
                                      'type (list-ref (cdr s) idx)))))
                     (throw (type-error e "Unknown union member {}" field)))))
                (f-list
                  (type-check-expr (make-expr
                                     file line col
                                     'field
                                     (or (index (cut eqv? <> field)
                                                f-list)
                                         (throw (type-error
                                                  e
                                                  "Reference to nonexistend field")))
                                     (list struct))))
                (else (throw (type-error
                               e "Type {} has no named fields"
                               (type-repr type)))))))))
        ((integer? field)
         (let ((type (let ((t (expr-type struct)))
                      (if (type-struct? t)
                        t
                        (type-struct t type-env)))))
           (if (type-struct? type)
             (if (< field (length type))
               (return (make-expr
                         file
                         line
                         col
                         'ref
                         field
                         (list struct)
                         'struct-type type
                         'type (list-ref type field)))
               (throw (type-error
                        e "Invalid struct index")))
             (throw (type-error
                      e "Expected struct, got {}" (type-repr type))))))
        (else (throw (type-error
                       e "Expected field name or index")))))))

(define (type-check-set e)
  (let+ (: sub init file line col) e
    (let+ (loc expr) sub
      (if init
        (do
          (expr <- (type-check-expr expr))
          (set-state `(data ,(expr-value loc) type)
                     (expr-type expr))
          (return (update-expr
                    e
                    'type 'void
                    'loc-type 'symbol
                    'value (expr-value loc)
                    'sub (list expr))))

        (do
          ((: type-env) <- (get-state))
          (loc <- (type-check-expr loc))
          (expr <- (type-check-expr expr))
          (cond
            ((type=? (expr-type loc)
                     (expr-type expr)
                     type-env)
             (case (expr-tag loc)
               ((symbol)
                (let ((ref-t (get '(ref ref) loc)))
                 (case ref-t
                   ((symbol-value)
                    (return (update-expr
                              e
                              'type 'void
                              'loc-type 'symbol
                              'value (expr-value loc)
                              'sub (list expr))))
                   ((closure)
                    (return (update-expr
                              e
                              'type 'void
                              'loc-type 'closure
                              'value (get '(ref value) loc)
                              'sub (list expr)))))))
               ((ref)
                (let ((t (let ((t (expr-type (car (sub-expr loc)))))
                          (if (type-struct? t)
                            t
                            (type-struct t type-env)))))
                  (return
                    (update-expr
                      e
                      'type 'void
                      'loc-type 'ref
                      'struct-type t
                      'ptr-type (list-ref t (expr-value loc))
                      'value (expr-value loc)
                      'sub (cons expr (sub-expr loc))))))
               (else
                 (throw (type-error loc "Expected symbol/field")))))
            ((type~? (expr-type loc)
                     '(|:| ptr *)
                     type-env)
             (do
               (type <- (ptr-type loc))
               (if (type=? type
                           (expr-type expr)
                           type-env)
                 (return (update-expr
                           e
                           'type 'void
                           'loc-type 'ref
                           'value 0
                           'sub (list expr loc)))
                 (throw (type-mismatch expr type (expr-type expr))))))))))))

(define (type-check-mov e)
  (let+ (val loc) (sub-expr e)
    (do
      (val <- (type-check-expr val))
      (loc <- (type-assert loc `(|:| ptr ,(expr-type val))))
      (return
        (make-expr-at
          e
          'set 0
          (list val loc)
          'type 'void
          'ptr-type (expr-type val)
          'loc-type 'ref)))))

(define (ptr-type e)
  (do
    ((: type-env) <- (get-state))
    (let ((type (lookup-type (expr-type e) type-env)))
     (if (type~? type '(|:| ptr *) type-env)
       (return (list-ref type 2))
       (throw (type-mismatch e '(|:| ptr *) type))))))

(define (type-check-deref e)
  (do
    (ptr <- (type-check-expr (car (sub-expr e))))
    ((: type-env) <- (get-state))
    (type <- (ptr-type ptr))
    (return
      (let ((const (and (expr-is? ptr 'offset)
                        (expr-value ptr))))
        (update-expr
          e
          'type type
          'ptr-type type
          'sub (if const
                 (sub-expr ptr)
                 (list ptr))
          'value const)))))

(define (type-check-offset e)
  (let+ (: (const value) sub) e
    (do
      (ptr <- (type-check-expr (car sub)))
      (type <- (ptr-type ptr))
      ((: type-env) <- (get-state))
      (offset <- (if const
                   (return)
                   (do
                     (i <- (type-check-expr (cadr sub)))
                     (return
                       (make-expr-at
                         e
                         'num-expr
                         '*
                         (list i (make-expr-at
                                   e
                                   'int
                                   (data-size type)
                                   '()
                                   'type 'int))
                         'type 'int)))))
      (return
        (update-expr
          e
          'value (and const (* const (data-size type)))
          'type (expr-type ptr)
          'sub (cons ptr
                     (if const
                       '()
                       (list offset))))))))

(define (type-check-is e)
  (let+ (: value sub) e
    (do
      (expr <- (type-check-expr (car sub)))
      ((: type-env) <- (get-state))
      (let* ((type (expr-type expr))
             (s (type-struct type type-env)))
        (and (assert (type-union? s)
                     (type-error e "Expected union"))
             (let* ((inf (get `(type-table ,(base-type type type-env))
                              type-env))
                    (union (and inf (get 'union inf))))
               (if (in? value union)
                 (return
                   (make-expr (expr-file e)
                              (expr-line e)
                              (expr-col e)
                              'cmp-expr
                              '=
                              (list (make-expr (expr-file e)
                                               (expr-line e)
                                               (expr-col e)
                                               'ref 0
                                               (list expr)
                                               'struct-type '(int (: ptr void))
                                               'type 'int)
                                    (make-expr (expr-file e)
                                               (expr-line e)
                                               (expr-col e)
                                               'int (index (cut eqv? value <>)
                                                           union)
                                               '()
                                               'type 'int))
                              'type 'bool))
                 (throw (type-error e "Unknown union member {}" value)))))))))

(define (type-check-quoted e)
  (let ((sub (car (sub-expr e))))
   (case (expr-tag sub)
     ((symbol)
      (throw 'not-implemented ""))
     (else
       (throw (type-error
                e
                "Expected symbol"))))))
