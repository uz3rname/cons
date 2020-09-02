;-unit syntax -uses prelude -uses lex -uses posix
(include "inc/macro.scm")
(import (chicken file))

(define (syntax-error expr . fmt)
  (apply expr-error 'syntax-error expr fmt))

(define (invalid-expr e)
  (syntax-error e "Invalid expression"))

(define (expr-error type expr . fmt)
  (make-error type
              "{} (expression: {}, {}, line: {}, col: {})"
              (apply format-str fmt)
              (expr-repr expr)
              (expr-file expr)
              (expr-line expr)
              (expr-col expr)))

(define (c-ify-name name)
  (string->symbol
    (translate-str
      '(("->" "_to_")
        ("-" "_")
        ("!" "_1_")
        ("." "__")
        ("?" "_q_")
        ("%" "_ps_")
        ("+" "_p_")
        ("*" "_a_")
        ("/" "_s_")
        ("=" "_eq_")
        (">" "_gt_")
        ("<" "_lt_"))
      (let* ((s (symbol->string name))
             (l (string-length s)))
        (if (char=? (string-ref s (- l 1))
                    #\?)
          (concat "is_" (substr s 0 (- l 1)))
          s)))))

(define (token-data x) (cadr x))
(define (token-tag x) (car x))
(define (token-line x) (caddr x))
(define (token-col x) (cadddr x))

(define (parser-error msg)
  (do
    ((: line col) <- (get-state))
    (throw 'parser-error
           "{} (line: {}, col: {})" msg line col)))

(define (next-token)
  (do
    ((: tokens) <- (get-state))
    (if (null? tokens)
      (return #f)
      (do
        (set-state 'tokens (cdr tokens))
        (set-state 'line (list-ref (car tokens) 2))
        (set-state 'col (list-ref (car tokens) 3))
        (return (car tokens))))))

(define (peek-token)
  (do
    ((: tokens) <- (get-state))
    (if (null? tokens)
      (return #f)
      (return (car tokens)))))

(define (parse-list end-tag)
  (do
    ((: stack) <- (get-state))
    (set-state 'stack
               (cons '() stack))
    (letrec
      ((loop (lambda ()
               (do
                 (token <- (peek-token))
                 (if token
                   (let+ (tag data) token
                     (if (eqv? tag end-tag)
                       (do
                         (next-token)
                         ((: stack result) <- (get-state))
                         (set-state 'stack
                                    (cdr stack))
                         (return (reverse (car stack))))
                       (do
                         (e <- (next-expr))
                         ((: stack) <- (get-state))
                         (set-state 'stack
                                    (cons (cons e (car stack))
                                          (cdr stack)))
                         (loop))))
                   (parser-error "Unexpected EOF"))))))
      (loop))))

(define (make-expr file line col tag value sub . rest)
  (append
    `((tag . ,tag)
      (file . ,file)
      (line . ,line)
      (col . ,col)
      (value . ,value)
      (sub . ,sub))
    (map* cons (group 2 rest))))

(define (make-expr-at e tag value sub . rest)
  (apply make-expr
         (expr-file e)
         (expr-line e)
         (expr-col e)
         tag
         value
         sub
         rest))

(define (update-expr e . rest)
  (foldl (lambda (e x)
           (let+ (k v) x
             (set k v e)))
         e
         (group 2 rest)))

(define (expr tag value sub)
  (do
    ((: line col file) <- (get-state))
    (return (make-expr file line col tag value sub))))

(define (expr-tag e)
  (get 'tag e))

(define (expr-is? e tag)
  (cond
    ((symbol? tag)
     (eqv? (expr-tag e) tag))
    ((pair? tag)
     (in? (expr-tag e) tag))
    (else #f)))

(define (expr-list-is? e sym)
  (and (expr-list? e)
       (expr? (car e))
       (let+ (: tag value) (car e)
         (and (eqv? tag 'symbol)
              (eqv? value sym)))))

(define (expr-file e)
  (if (expr-list? e)
    (expr-file (car e))
    (get 'file e)))

(define (expr-line e)
  (if (expr-list? e)
    (expr-line (car e))
    (get 'line e)))

(define (expr-col e)
  (if (expr-list? e)
    (expr-col (car e))
    (get 'col e)))

(define (expr-value e)
  (get 'value e))

(define (sub-expr e)
  (get 'sub e))

(define (set-sub-expr e sub)
  (set 'sub sub e))

(define (next-expr)
  (define (parse-int s r)
    (let ((x (string->int (substr s 1 (string-length s)) r)))
     (if x (expr 'int x '())
       (lexical-error "Invalid number representation."))))
  (do
    (token <- (next-token))
    (if (not token)
      (return (void))
      (do
        (let+ (tag data line col) token
          (case tag
            ((number)
             (expr (if (string-search "." data)
                     'real
                     'int)
                   (string->number data)
                   '()))
            ((char)
             (expr 'char data '()))
            ((symbol)
             (scase data
               (("$")
                (do
                  (n <- (peek-token))
                  (if (and n (eqv? (token-tag n) 'string))
                    (do
                      (next-token)
                      (h <- (expr 'symbol
                                  '$cstring
                                  '()))
                      (s <- (expr 'string
                                  (token-data n)
                                  '()))
                      (return (list h s)))
                    (expr 'symbol (string->symbol data) '()))))
               (else (expr 'symbol (string->symbol data) '()))))
            ((string)
             (expr 'string data '()))
            ((open-par)
             (parse-list 'close-par))
            ((open-br)
             (do
               (e <- (parse-list 'close-br))
               (expr 'list '() e)))
            ((close-par close-br)
             (parser-error "Unmatched parens"))
            ((quote)
             (do
               (e <- (next-expr))
               (expr 'quoted #t (list e))))
            ((hash)
             (do
               (n <- (next-token))
               (let+ (tag data) n
                 (case tag
                   ((symbol)
                    (scase data
                      (("t") (expr 'bool #t '()))
                      (("f") (expr 'bool #f '()))
                      (("$") (>>=
                               (expr 'symbol 'declare '())
                               (lambda (e)
                                 (return
                                   (set 'preproc #t e)))))
                      (else
                        (case (string-ref data 0)
                          ((#\x) (parse-int data 16))
                          ((#\b) (parse-int data 2))
                          ((#\0) (parse-int data 8))
                          (else (lexical-error "Unrecognized token"))))))
                   ((open-par)
                    (do
                      (e <- (parse-list 'close-par))
                      (expr 'struct '() e)))
                   ((open-br)
                    (do
                      (e <- (parse-list 'close-br))
                      (expr 'array '() e)))
                   (else (lexical-error "Unrecognized token."))))))
            (else
              (lexical-error "Unrecognized token"))))))))

(define (parse)
  (do
    (e <- (next-expr))
    ((: result) <- (get-state))
    (if (void? e)
      (return (reverse result))
      (do
        (set-state 'result (cons e result))
        (parse)))))

(define (parser-state lst filename)
  (table (tokens lst)
         (file filename)
         (stack '())
         (result '())))

(define (parse-tokens lst filename)
  (let-values (e _) ((parse) (parser-state lst
                                           filename))
    e))

(define (expr? e)
  (and (list? e)
       (alist? e)
       (let+ (: tag) e
         (in? tag
              '(bool int real string char null void closure
                     symbol array list c-array struct type
                     call num-expr cmp-expr bool-expr function 
                     fn def extern let typedef is decl
                     if cast sequence macrodef ref set field
                     deref offset)))))

(define (expr-list? e)
  (and (pair? e)
       (or (expr? (car e))
           (expr-list? (car e)))))

(define (valid-lambda-list? lst)
  (or (null? lst)
      (and (expr-list? lst)
           (all (cut expr-is? <> 'symbol)
                lst))))

(define (transform-lambda-list lst)
  (map expr-value
       (accept valid-lambda-list?
               lst
               (syntax-error lst "Invalid lambda list."))))

(define (make-fn-expr file line col arg-list head tail)
  (let* ((arg-list (transform-lambda-list arg-list))
         (head (transform-expr head))
         (tail (transform-sub-expr tail))
         (type (if (expr-is? head 'type)
                 (expr-value head)
                 (make-template-function (length arg-list))))
         (body (if (expr-is? head 'type)
                 tail
                 (cons head tail))))
    (make-expr file line col 'fn (list arg-list type) body)))

(define (transform-fn e)
  (let+ (sym arg-list head . tail) e
    (make-fn-expr (expr-file sym)
                  (expr-line sym)
                  (expr-col sym)
                  arg-list
                  head
                  tail)))

(define (transform-if e)
  (and (assert (= (length e) 4)
               (syntax-error
                 e
                 "Invalid conditional expression"))
       (make-expr
         (expr-file e)
         (expr-line e)
         (expr-col e)
         'if
         '()
         (transform-sub-expr (cdr e)))))

(define (transform-def e)
  (let+ (sym name . rest) e
    (let+ (: line col file) sym
      (and (assert (expr-is? name 'symbol)
                   (syntax-error
                     name  
                     "Expected symbol"))
           (if (= (length e) 3)
             (make-expr
               file
               line
               col
               'def
               (expr-value name)
               (list (transform-expr (car rest))))
             (if (valid-lambda-list? (car rest))
               (let+ (arg-list head . tail) rest
                 (make-expr
                   file
                   line
                   col
                   'def
                   (expr-value name)
                   (list (make-fn-expr file
                                       line
                                       col
                                       arg-list
                                       head
                                       tail))))
               (map (lambda (x)
                      (let+ (arg-list head . tail) x
                        (make-expr file
                                   line
                                   col
                                   'def
                                   (expr-value name)
                                   (list (make-fn-expr
                                           file
                                           line
                                           col
                                           arg-list
                                           head
                                           tail)))))
                    rest)))))))

(define (transform-extern e)
  (let+ (sym name . funcs) e
    (make-expr
      (expr-file sym)
      (expr-line sym)
      (expr-col sym)
      'extern
      (cons (expr-value name)
            (map (lambda (x)
                   (let ((x (map transform-expr x)))
                    (and (assert (>= (length x) 1)
                                 (syntax-error x "Invalid declaration"))
                         (let+ (type . f-name) x
                           (and (assert (expr-is? type 'type)
                                        (syntax-error
                                          type
                                          "Expected type"))
                                (assert (or (null? f-name)
                                            (expr-is? (car f-name) 'string))
                                        (syntax-error
                                          (car f-name)
                                          "Expected string"))
                                (list (expr-value type)
                                      (if (null? f-name)
                                        (symbol->string
                                          (c-ify-name
                                            (expr-value name)))
                                        (expr-value (car f-name)))))))))
                 funcs))
      '())))

(define (transform-sequence e lst)
  (if (null? lst)
    (make-expr-at e 'void '() '())
    (make-expr-at
      e
      'sequence
      #t
      (transform-sub-expr lst))))

(define (transform-type e)
  (define (transform-type e)
    (cond ((pair? e)
           (if (expr-list? e)
             (map transform-type e)
             (case (expr-tag e)
               ((list array)
                `(|:| ,(expr-tag e) . ,(transform-type
                                         (sub-expr e))))
               ((struct)
                (transform-type (sub-expr e)))
               (else (transform-type
                       (expr-value e))))))
          ((and (symbol? e)
                (eqv? (string-ref (symbol->string e) 0)
                      #\*))
           (list '|:| 'ptr
                 (string->symbol
                   (substr (symbol->string e) 1))))
          (else e)))
  (make-expr
    (expr-file e)
    (expr-line e)
    (expr-col e)
    'type
    (unlist (transform-type (cdr e)))
    '()))

(define (transform-typedef e)
  (let+ (sym name . t) e
    (let ((type (transform-expr (cons (make-expr
                                        (expr-file name)
                                        (expr-line name)
                                        (expr-col name)
                                        'symbol
                                        '|::|
                                        '())
                                      t))))
      (and
        (assert (or (expr-is? name 'symbol)
                    (and (expr-list? name)
                         (all (cut expr-is? <> 'symbol)
                              name)))
                (syntax-error name "Expected symbol"))
        (assert (expr-is? type 'type)
                (syntax-error type "Expected type"))
        (make-expr
          (expr-file e)
          (expr-line e)
          (expr-col e)    
          'typedef
          (list
            (if (expr-list? name)
              (map expr-value name)
              (expr-value name))
            (expr-value type)
            (eqv? (expr-value sym) 'alias))
          '())))))

(define (transform-decl e)
  (define (invalid-decl)
    (syntax-error e "Invalid declaration"))
  (define (tform-decl e)
    (cond ((expr-list? e)
           (map tform-decl e))
          ((expr-is? e '(symbol string))
           (expr-value e))
          (else (throw (invalid-decl)))))
  (let+ (sym . tail) e
    (make-expr
      (expr-file e)
      (expr-line sym)
      (expr-col sym)
      'decl
      (let ((d (map tform-decl tail)))
       (if (get 'preproc sym)
         (list d)
         d))
      '())))

(define (transform-cast e)
  (and (assert (> (length e) 3)
               (invalid-expr e))
       (let+ (sym what . type) e
         (let ((type (transform-expr type)))
          (and (assert (expr-is? type 'type)
                       (invalid-expr e))
               (make-expr
                 (expr-file e)
                 (expr-line sym)
                 (expr-col sym)
                 'cast
                 (expr-value type)
                 (list (transform-expr what))))))))

(define (transform-ref e)
  (and (assert (= (length e) 3)
               (invalid-expr e))
       (let+ (sym struct idx) e
         (and (assert (and (expr-is? idx 'int)
                           (>= (expr-value idx) 0))
                      (syntax-error e "Invalid struct index"))
              (make-expr
                (expr-file e)
                (expr-line sym)
                (expr-col sym)
                'ref
                (expr-value idx)
                (list (transform-expr struct)))))))

(define (transform-field e)
  (and (assert (= (length e) 3)
               (invalid-expr e))
       (let+ (sym e field) e
         (and (assert (or (expr-is? field 'symbol)
                          (expr-is? field 'int))
                      (syntax-error field
                                    "Expected symbol"))
              (make-expr
                (expr-file e)
                (expr-line sym)
                (expr-col sym)
                'field
                (expr-value field)
                (list (transform-expr e)))))))

(define (transform-is e)
  (and (assert (= (length e) 3)
               (invalid-expr e))
       (let+ (sym value type) e
         (let ((sym (transform-expr sym))
               (value (transform-expr value))
               (type (transform-expr type)))
           (and (assert (and (expr-is? type 'symbol)
                             (field-sym? (expr-value type)))
                        (syntax-error type "Expected type"))
                (make-expr
                  (expr-file e)
                  (expr-line sym)
                  (expr-col sym)
                  'is
                  (field-name (expr-value type))
                  (list value)))))))

(define (transform-set e)
  (let+ (loc expr) (map transform-expr (cdr e))
    (and (assert (expr-is? loc '(symbol field))
                 (syntax-error e "Expected symbol/field"))
         (make-expr (expr-file e)
                    (expr-line e)
                    (expr-col e)
                    'set
                    #t
                    (list loc expr)))))

(define (transform-mov e)
  (and (assert (= (length e) 3)
               (syntax-error e "Expected two arguments"))
       (make-expr-at
         e
         'mov
         #t
         (map transform-expr (cdr e)))))

(define (transform-deref e)
  (and (assert (= (length e) 2)
               (syntax-error e "Expected single argument"))
       (make-expr (expr-file e)
                  (expr-line e)
                  (expr-col e)
                  'deref
                  #t
                  (list (transform-expr (cadr e))))))

(define (transform-offset e)
  (and (assert (= (length e) 3)
               (syntax-error e "Expected two arguments"))
       (let+ (sym ptr o) e
         (make-expr (expr-file sym)
                    (expr-line sym)
                    (expr-col sym)
                    'offset
                    (and (expr-is? o 'int)
                         (expr-value o))
                    (cons (transform-expr ptr)
                          (if (expr-is? o 'int)
                            '()
                            (list (transform-expr o))))))))

(define (special-forms-dispatch)
  (list
    'fn transform-fn
    'begin (lambda (e)
             (transform-sequence (car e) (cdr e)))
    'if transform-if
    'def transform-def
    'extern transform-extern
    '|::| transform-type
    'type transform-typedef
    'alias transform-typedef
    'declare transform-decl
    '-> transform-cast
    (string->symbol ".") transform-field
    'set! transform-set
    'mov! transform-mov
    'is transform-is
    'deref transform-deref
    'offset transform-offset))

(define (builtin-forms)
  (map car (group 2 (special-forms-dispatch))))

(define (special-form? e)
  (let+ (head . tail) e
    (and (expr? head)
         (expr-is? head 'symbol)
         (and-let* ((x (memv (expr-value head)
                             (special-forms-dispatch))))
           ((cadr x) e)))))

(define (expr-predicate e tag valid-syms)
  (let+ (head . tail) e
    (and (expr? head)
         (expr-is? head 'symbol)
         (memv (expr-value head)
               valid-syms)
         (make-expr
           (expr-file e)
           (expr-line head)
           (expr-col head)     
           tag
           (expr-value head)
           (transform-sub-expr tail)))))

(define (num-expr? e)
  (expr-predicate e
                  'num-expr
                  `(+ - * / mod shl shr logand logor xor)))

(define (cmp-expr? e)
  (expr-predicate e
                  'cmp-expr '(eq neq = /= < > <= >=)))

(define (bool-expr? e)
  (expr-predicate e
                  'bool-expr '(and or not)))

(define (field-ref? e)
  (and (expr-list? e)
       (let+ (head . tail) e
         (and (expr? head)
              (expr-is? head 'symbol)
              (field-sym? (expr-value head))
              (transform-field
                (cons (make-expr (expr-file head)
                                 (expr-line head)
                                 (expr-col head)
                                 'symbol 'field '())
                      (append tail
                              (list (make-expr-at
                                      e
                                      'symbol
                                      (field-name (expr-value head))
                                      '())))))))))

(define (transform-expr e)
  (if (or (null? e)
          (expr-list? e))
    (transform-list e)
    (let+ (: file line col tag sub) e
      (case tag
        ((array struct list)
         (make-expr
           file
           line
           col
           tag
           '()
           (transform-sub-expr sub)))
        (else e)))))

(define (transform-sub-expr args)
  (foldr (lambda (x xs)
           (if (expr-list? x)
             (append x xs)
             (cons x xs)))
         '()
         (map transform-expr args)))

(define (make-call-expr e)
  (make-expr
    (expr-file e)
    (expr-line e)
    (expr-col e)
    'call
    #t
    (transform-sub-expr e)))

(define (make-void e)
  (make-expr
    (expr-file e)
    (expr-line e)
    (expr-col e)
    'void
    #t
    '()))

(define (transform-list e)
  (or (and (null? e)
           (make-void e))
      (special-form? e)
      (num-expr? e)
      (cmp-expr? e)
      (bool-expr? e)
      (field-ref? e)
      (make-call-expr e)))

(define (parse-string s filename dirs)
  (parse-tokens (tokenize s) filename))

(define (parse-file f dirs)
  (and (assert (file-exists? f)
               'error "Couldn't find {}" f)
       (parse-string (file->string f)
                     f
                     (cons (file-directory f)
                           dirs))))

(define (expr-repr e)
  (cond
    ((null? e) "()")
    ((expr-list? e)
     (concat "(" (join-str " " (map expr-repr e)) ")"))
    (else
      (let+ (: tag value sub) e
        (case tag
          ((unit)
           (format-str
             "File: {} \n {}"
             value
             (expr-repr sub)))
          ((int real bool function)
           (repr value))
          ((closure)
           (format-str
             "(closure {} ({}))"
             value
             (expr-repr sub)))
          ((symbol)
           (if (symbol? value)
             (repr value)
             (format-str "(ref {} {})"
                         (get '(ref ref) e)
                         (get '(ref value) e))))
          ((char)
           (fmt #f "#\\x{}" (number->string (char->integer value) 16)))
          ((string)
           (concat "\"" value "\""))
          ((num-expr cmp-expr bool-expr)
           (format-str
             "({} {})"
             value
             (join-str " " (map expr-repr sub))))
          ((sequence)
           (format-str
             "(begin {})"
             (join-str " " (map expr-repr sub))))
          ((call)
           (format-str
             "({} {})"
             (expr-repr (car sub))
             (join-str " " (map expr-repr (cdr sub)))))
          ((extern)
           (let+ (symbol type) value
             (format-str
               "(extern {})"
               symbol)))
          ((def)
           (format-str
             "(def {} {})"
             value
             (join-str " " (map expr-repr sub))))
          ((fn)
           (let+ (arg-list type) value
             (format-str
               "(fn ({}) (:: {}) {})"
               (join-str " " (map repr arg-list))
               (join-str " " (map type-repr type))
               (join-str " " (map expr-repr sub)))))
          ((list)
           (concat "["
                   (join-str " " (map expr-repr sub))
                   "]"))
          ((null) "[]")
          ((array)
           (concat "#["
                   (join-str " " (map expr-repr sub))
                   "]"))
          ((struct)
           (concat "#("
                   (join-str " " (map expr-repr sub))
                   ")"))
          ((if)
           (format-str
             "(if {})"
             (join-str " " (map expr-repr sub))))
          ((let)
           (format-str
             "(let ({}) {})"
             (join-str
               " "
               (map (lambda (name body)
                      (format-str
                        "({} {})"
                        name
                        (join-str " " (map expr-repr (sub-expr body)))))
                    value
                    (list-head sub (length value))))
             (join-str " " (map expr-repr (list-tail sub (length value))))))
          ((type)
           (type-repr value))
          ((typedef)
           (let+ (symbol type) value
             (format-str
               "(type {} {})"
               symbol
               (type-repr type))))
          ((cast)
           (format-str
             "(-> {} :: {})"
             (expr-repr (car sub))
             (type-repr value)))
          ((ref)
           (format-str
             "(ref {} {})"
             (expr-repr (car sub))
             value))
          ((void)
           "()")
          ((set)
           (format-str
             "(set! {} {})"
             (expr-repr (car sub))
             (expr-repr (cadr sub))))
          ((field)
           (format-str
             "(. {} {})"
             (expr-repr (car sub))
             value))
          ((is)
           (format-str
             "(is {} .{})"
             (expr-repr (car sub))
             value))
          ((decl)
           (format-str
             "(declare {})"
             value))
          ((deref)
           (format-str
             "(deref {})"
             (expr-repr (car sub))))
          ((offset)
           (format-str
             "(offset {} {})"
             (expr-repr (car sub))
             (or value (expr-repr (cadr sub)))))
          ((mov)
           (format-str
             "(mov! {} {})"
             (expr-repr (car sub))
             (expr-repr (cadr sub))))
          ((c-array)
           (format-str
             "$[{}]"
             (join-str " " (map expr-repr sub))))
          (else (throw 'error "Invalid expression tag {}" e)))))))

(define (make-expr-from e file line col)
  (define (tform x)
    (cond ((list? x)
           (if (null? x)
             (make-expr file line col 'void #t '())
             (case (car x)
               ((if)
                (make-expr file line col
                           'if #t
                           (map tform (cdr x))))
               (else
                 (make-expr file line col
                            'call
                            #t
                            (cons (tform (car x))
                                  (map tform (cdr x))))))))
          ((symbol? x)
           (make-expr file line col
                      'symbol
                      x
                      '()))
          ((string? x)
           (make-expr file line col
                      'string
                      x
                      '()))
          ((integer? x)
           (make-expr file line col
                      'int
                      x
                      '()))
          ((number? x)
           (make-expr file line col
                      'real
                      x
                      '()))))
  (tform e))
