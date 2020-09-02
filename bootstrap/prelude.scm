;-unit prelude -uses posix
(include "inc/macro.scm")
(import (chicken process-context)
        (chicken file))

(define (make-error type . fmt)
  (table (error type)
         (msg (apply format-str fmt))))

(define (error? x)
  (and (alist? x)
       (bool (assv 'error x))))

(define (error-type e)
  (get 'error e))
(define (error-msg e)
  (get 'msg e))

(define (print-error e)
  (print
    (format-str
      "Error({}): {}"
      (error-type e)
      (error-msg e))))

(define (default-handler err)
  (print-error err)
  (exit 1))

(define handler-stack (list))

(define (throw arg . data)
  (let ((e (if (error? arg)
             arg
             (apply make-error arg data))))
    (if (null? handler-stack)
      (default-handler e)
      (let ((f (car handler-stack)))
       (set! handler-stack
         (cdr handler-stack))
       (f e)))))

(define try
  (case-lambda
    ((proc handler)
     (try proc handler (lambda () (void))))
    ((proc handler cleanup)
     (let ((abnormal #t))
      (call/cc
        (lambda (k)
          (dynamic-wind
            (lambda ()
              (set! handler-stack
                (cons (lambda (err)
                        (cleanup)
                        (k (handler err)))
                      handler-stack)))
            (lambda ()
              (let ((res (proc)))
               (set! abnormal #f)
               res))
            (lambda ()
              (unless abnormal
                (set! handler-stack
                  (cdr handler-stack)))
              (cleanup)))))))))

(define (print x . xs)
  (display x)
  (cond ((null? xs)
         (newline)
         x)
        (else (display #\space)
              (apply print xs))))

(define (print1 x)
  (display x)
  (newline)
  x)

(define (println x . args)
  (apply display x args)
  (apply newline args))

(define substr
  (case-lambda
    ((s start end)
     (substring s start end))
    ((s start)
     (substring s start (string-length s)))))

(define (format-str fmt . args)
  (let loop ((result '())
             (start 0)
             (args args))
    (let ((pos (string-search "{}" fmt start)))
     (if pos
       (if (null? args)
         #f
         (loop (cons* (repr (car args))
                      (substr fmt start pos) 
                      result)
               (+ pos 2)
               (cdr args)))
       (if (null? args)
         (apply concat
                (reverse (cons (substr fmt
                                          start
                                          (string-length fmt))
                               result)))
         #f)))))

(define (fmt out fmt . args)
  (let ((str (apply format-str fmt args)))
   (case out
     ((#t) (display str))
     ((#f) str)
     (else (display str out)))))

(define (read-line in)
  (let loop ((r '())
             (ch (read-char in)))
    (cond ((and (eof-object? ch)
                (null? r))
           ch)
          ((or (eof-object? ch)
               (char=? ch #\newline))
           (list->string (reverse r)))
          (else (loop (cons ch r)
                      (read-char in))))))

(define (read-contents in)
  (let loop ((r '())
             (ch (read-char in)))
    (if (eof-object? ch)
      (list->string (reverse r))
      (loop (cons ch r)
            (read-char in)))))

(define (file->string filename)
  (let ((in (open-input-file filename)))
   (let loop ((ch (read-char in))
              (result '()))
     (if (eof-object? ch)
       (begin
         (close-input-port in)
         (list->string (reverse result)))
       (loop (read-char in)
             (cons ch result))))))

(define (write-to-file x file)
  (let ((out (open-output-file file)))
   (write x out)
   (close-output-port out)))

(define (read-lines filename)
  (split-str "\n" (file->string filename)))

(define (repr x)
  (fcase x
    ((string?) x)
    ((char?) (make-string 1 x))
    ((number?)
     (number->string x))
    ((boolean?)
     (if x "#t" "#f"))
    ((list? pair?)
     (list-repr x))
    ((symbol?)
     (symbol->string x))
    ((vector?)
     (concat "#("
             (join-str " "
                       (map repr (vector->list x)))
             ")"))
    (else "<unspecified>")))

(define (list-repr x)
  (letrec
    ((loop
       (lambda (x result)
         (if (pair? x)
           (loop (cdr x)
                 (cons (repr (car x))
                       result))
           (join-str
             " "
             (reverse
               (if (null? x)
                 result
                 (append (list (repr x) ".")
                         result))))))))
    (string-append
      "("
      (loop x '())
      ")")))

(define (key-arg? x)
  (and (> (string-length x) 1)
       (char=? (string-ref x 0)
               #\-)))

(define (parse-argv argv dict)
  (let ((rules
          (map (lambda (x)
                 (let+ (_ name) x
                   (cons name
                         (and (>= (length x) 6)
                              (list-ref x 5)))))
               dict)))  
    (define (iter argv result)
      (define (parse-arg head tail)
        (let ((arg (find (comp (conj (if (string=? head "")
                                       (const #t)
                                       (! (cut string=? <> "")))
                                     (cut starts-with <> head))
                               car)
                         (sort dict
                               (lambda (x y)
                                 (> (string-length (car x))
                                    (string-length (car x))))))))
          (if arg
            (let+ (key name type) arg
              (let ((value (if (equal? head key)
                             (if (null? tail)
                               #f
                               (car tail))
                             (substr head (string-length key))))
                    (rest (if (and (equal? head key)
                                   (not (in? type '(flag neg-flag))))
                            (if (null? tail)
                              '()
                              (cdr tail))
                            tail)))
                (case type
                  ((flag neg-flag)
                   (if (not (equal? head key))
                     (throw 'error "Unknown argument -{}" head)
                     (let ((rule (get name rules)))
                      (cond
                        ((and rule
                              (eqv? (car rule) 'disjoint)
                              (any (cut get <> result)
                                   (cdr rule)))
                         (throw
                           'error
                           "Argument -{} cannot be combined with {}"
                           head
                           (join-str
                             ", "
                             (map (lambda (x)
                                    (concat
                                      "-"
                                      (car (find (comp (cut eqv? <> x)
                                                       cadr)
                                                 dict))))
                                  (cdr rule)))))
                        (else (iter rest
                                    (set name
                                         (eqv? type 'flag)
                                         result)))))))
                  ((single)
                   (if value
                     (iter rest
                           (set name
                                value
                                result))
                     (throw 'error "Argument required after -{}" head)))
                  ((multiple)
                   (if value
                     (iter rest
                           (push-back name
                                      value
                                      result))
                     (throw 'error "Argument required after -{}" head)))
                  (else #f))))
            (throw 'error "Unknown argument: -{}" head))))
      (if (null? argv)
        (map (lambda (x)
               (let+ (name . value) x
                 (if (eqv? (list-ref (find (comp (cut eqv? name <>)
                                                 cadr)
                                           dict)
                                     2)
                           'multiple)
                   (cons name (reverse value))
                   x)))
             result)
        (let+ (head . tail) argv
          (if (key-arg? head)
            (parse-arg (substr head 1)
                       tail)
            (parse-arg "" argv)))))
    (iter argv
          (map (lambda (x)
                 (let+ (_ name type) x
                   (cons name
                         (or (and (>= (length x) 5)
                                  (list-ref x 4))
                             (case type
                               ((multiple) '())
                               ((single flag) #f)
                               ((neg-flag) #t))))))
               dict))))

(define (show-help argv dict)
  (fmt #t "Usage:\n    {} <{}>"
          (car argv)
          (cadr (find (comp empty-string? car)
                      dict)))
  (for-each (lambda (x)
              (fmt #t " ")
              (let+ (arg name type) x
                (case type
                  ((flag neg-flag)
                   (fmt #t "[-{}]" arg))
                  ((single)
                   (fmt #t "[-{} {}]" arg name))
                  ((multiple)
                   (fmt #t "[-{} {} ...]" arg name)))))
            (filter (! (comp empty-string? car)) dict)) 
  (fmt #t "\nOptions:\n")
  (let ((len (+ (foldr max
                       0
                       (map (comp string-length car)
                            dict))
                1)))
    (for-each (lambda (x)
                (let+ (arg _ _ desc) x
                  (unless (empty-string? arg)
                    (fmt #t "    -{} {}\n"
                            (ljust arg len)
                            desc))))
              (filter (! (comp empty-string? car)) dict))))

(define (foldr f init xs)
  (if (not (pair? xs))
    init
    (f (car xs)
       (foldr f init (cdr xs)))))

(define (foldl f init xs)
  (let loop ((res init)
             (xs xs))
    (if (null? xs)
      res
      (loop (f res (car xs))
            (cdr xs)))))

(define (unfoldr f x)
  (let loop ((res '())
             (b x))
    (call-with-values
      (lambda () (f b))
      (lambda (a . args)
        (if (null? args)
          (reverse res)
          (loop (cons a res)
                (car args)))))))

(define (unfoldl f x)
  (let loop ((res '())
             (b x))
    (call-with-values
      (lambda () (f b))
      (lambda (b . args)
        (if (null? args)
          res
          (loop (cons (car args) res)
                b))))))

(define (sum xs)
  (foldr + 0 xs))

(define (mklist x)
  (if (list? x)
    x
    (list x)))

(define (unlist x)
  (if (pair? x)
    (if (null? (cdr x))
      (car x)
      x)
    x))

(define (filter p xs)
  (foldr (lambda (head tail)
           (if (p head)
             (cons head tail)
             tail))
         '()
         xs))

(define remove
  (opt-lambda (x xs & (f eqv?))
    (filter (! (cut f x <>))
            xs)))

(define (reducel f init xs)
  (if (null? xs)
    init
    (foldl f
           (car xs)
           (cdr xs))))

(define (reducer f init xs)
  (if (null? xs)
    init
    (let ((lst (reverse xs)))
     (foldr f
            (car lst)
            (reverse (cdr lst))))))

(define (all p l . ls)
  (define (loop xs)
    (let ((ns (map null? xs)))
     (if (memv #t ns)
       (not (memv #f ns))
       (and (apply p (map car xs))
            (loop (map cdr xs))))))
  (loop (cons l ls)))

(define (any p l . ls)
  (define (loop xs)
    (let ((ns (map null? xs)))
     (if (memv #t ns)
       #f
       (or (apply p (map car xs))
           (loop (map cdr xs))))))
  (loop (cons l ls)))

(define (index p lst)
  (let loop ((i 0)
             (lst lst))
    (if (null? lst)
      #f
      (if (p (car lst))
        i
        (loop (+ i 1) (cdr lst))))))

(define (find pred lst)
  (if (null? lst)
    #f
    (if (pred (car lst))
      (car lst)
      (find pred (cdr lst)))))

(define in?
  (opt-lambda (x xs & (f eqv?))
    (any (cut f x <>) xs)))

(define (count p xs)
  (foldr (lambda (a b)
           (+ b 1))
         0
         (filter p xs)))

(define (zip x . xs)
  (let loop ((result '())
             (rest (cons x xs)))
    (if (any (disj null? not) rest)
      (reverse result)
      (loop (cons (map car rest)
                  result)
            (map cdr rest)))))

(define (unzip lst)
  (if (or (null? lst)
          (null? (car lst)))
    '()
    (cons (map car lst)
          (unzip (map cdr lst)))))

(define (group n lst)
  (let loop ((result '())
             (lst lst))
    (let+ (head tail) (split-at n lst)
      (if (null? head)
        (reverse result)
        (loop (cons head result)
              tail)))))

(define enumerate
  (opt-lambda (xs & (s 0))
    (let loop ((i s)
               (xs xs)
               (result '()))
      (if (null? xs)
        (reverse result)
        (loop (+ i 1)
              (cdr xs)
              (cons (list i (car xs))
                    result))))))

(define repeat
  (case-lambda
    ((x n)
     (let loop ((i n)
                (result '()))
       (if (zero? i)
         result
         (loop (- i 1)
               (cons x result)))))
    ((x)
     (let ((result (list x)))
      (set-cdr! result result)
      result))))

(define (map* f x . xs)
  (apply map
         (lambda (x . xs)
           (apply f (append x xs)))
         x
         xs))

(define (range . args)
  (define (range% s e d)
    (if (>= s e)
      '()
      (cons s (range% (+ s d) e d))))
  (case (length args)
    ((1)
     (range% 0 (car args) 1))
    ((2)
     (range% (car args) (cadr args) 1))
    ((3)
     (apply range% args))))

(define (mappend f lst)
  (foldr append
         '()
         (map f lst)))

(define (join d xs)
  (reducer (lambda (x xs)
             (append x d xs))
           '()
           xs))

(define (replace p f lst)
  (foldr (lambda (x tail)
           (cons (if (p x)
                   (f x)
                   x)
                 tail))
         '()
         lst))

(define (replace-ref lst i x)
  (append (list-head lst i)
          (cons x (list-tail lst (+ i 1)))))

(define (replace-last lst x)
  (foldr (lambda (head tail)
           (cons (if (null? tail)
                   x
                   head)
                 tail))
         '()
         lst))

(define (insert lst i x)
  (append (list-head lst i)
          (cons x (list-tail lst i))))

(define (cons* x . xs)
  (if (null? xs)
    x
    (if (null? (cdr xs))
      (cons x (car xs))
      (cons* x (apply cons* xs)))))

(define (last-pair lst)
  (if (null? lst)
    '()
    (if (null? (cdr lst))
      lst
      (last-pair (cdr lst)))))

(define (last-car lst)
  (car (last-pair lst)))

(define (list-head lst k)
  (let loop ((result '())
             (tail lst)
             (k k))
    (if (or (zero? k)
            (null? tail))
      (reverse result)
      (loop (cons (car tail)
                  result)
            (cdr tail)
            (- k 1)))))

(define (but-last lst)
  (reverse (cdr (reverse lst))))

(define (take-while p xs)
  (let loop ((r '())
             (xs xs))
    (if (or (null? xs)
            (not (p (car xs))))
      (reverse r)
      (loop (cons (car xs) r)
            (cdr xs)))))

(define (drop-while p xs)
  (if (or (null? xs)
          (not (p xs)))
    xs
    (drop-while p (cdr xs))))

(define (sub-lists xs)
  (foldr (lambda (x xs)
           (cons (cons x (car xs)) xs))
         '(())
         xs))

(define sort
  (opt-lambda (xs & (f <) (k id))
    (if (or (null? xs)
            (null? (cdr xs)))
      xs
      (let ((p (car xs)))
       (let+ (l1 l2) (split-by (comp (cut f <> (k p))
                                     k)
                               (cdr xs))
         (append (sort l1 f k)
                 (cons p (sort l2 f k))))))))

(define unique
  (opt-lambda (xs & (f eqv?))
    (reverse (foldl (lambda (xs x)
                      (if (find (cut f x <>) xs)
                        xs
                        (cons x xs)))
                    '()
                    xs))))

(define (bsort f lst)
  (define (inner lst)
    (foldr (lambda (a b)
             (if (null? b)
               (cons a b)
               (if (f (car b) a)
                 (cons (car b)
                       (cons a (cdr b)))
                 (cons a b))))
           '()
           lst))
  (if (null? lst)
    '()
    (let ((s (inner lst)))
     (cons (car s)
           (bsort f (cdr s))))))

(define tree-search
  (opt-lambda (p tree & r)
    (letrec
      ((%search
         (lambda (x)
           (if (p x)
             (if r
               (append (list x)
                       (if (pair? (car x))
                         (%search (car x))
                         '())
                       (if (pair? (cdr x))
                         (%search (cdr x))
                         '()))
               (list x))
             (if (pair? x)
               (append (if (pair? (car x))
                         (%search (car x))
                         '())
                       (if (pair? (cdr x))
                         (%search (cdr x))
                         '()))
               '())))))
      (foldr (lambda (x tail)
               (append (if (pair? x)
                         (%search x)
                         '())
                       tail))
             '()
             tree))))

(define (flatten lst)
  (foldr (lambda (x tail)
           (if (list? x)
             (foldr cons
                    tail
                    (flatten x))
             (cons x tail)))
         '()
         lst))

(define (tree-replace p f tree)
  (define (%replace x)
    (if (p x)
      (f x)
      (if (pair? x)
        (cons (%replace (car x))
              (%replace (cdr x)))
        x)))
  (foldr (lambda (x tail)
           (cons (%replace x) tail))
         '()
         tree))

(define (split-by p lst)
  (foldr (lambda (next lst)
           (if (p next)
             (cons (cons next (car lst))
                   (cdr lst))
             (list (car lst)
                   (cons next (cadr lst)))))
         '(() ())
         lst))

(define (split-on p lst)
  (foldr (lambda (next lst)
           (if (p next)
             (cons '() lst)
             (cons (cons next (car lst))
                   (cdr lst))))
         '(())
         lst))

(define (split-at i lst)
  (let loop ((l1 '())
             (l2 lst)
             (i i))
    (if (zero? i)
      (list (reverse l1)
            l2)
      (if (null? l2)
        (list '() '())
        (loop (cons (car l2) l1)
              (cdr l2)
              (- i 1))))))

(define (split-while p lst)
  (let loop ((l1 '())
             (l2 lst))
    (if (or (null? l2)
            (not (p (car l2))))
      (list (reverse l1) l2)
      (loop (cons (car l2) l1)
            (cdr l2)))))

(define (cons-list to from)
  (foldl (lambda (a b) (cons b a))
         to
         from))

(define (cons-back xs x)
  (append xs (list x)))

(define call/cc call-with-current-continuation)
(define (void) (if #f #t))
(define (void? x) (eq? x (void)))

(define (id x) x)

(define (bool x)
  (not (not x)))

(define (and* x . xs)
  (if (null? xs)
    x
    (if x
      (apply and* xs)
      #f)))

(define (or* x . xs)
  (if (or x (null? xs))
    x
    (apply or* xs)))

(define (xor a b)
  (or (and a (not b))
      (and b (not a))))

(define (all* f . xs)
  (or (null? xs)
      (and (f (car xs))
           (apply all* f (cdr xs)))))

(define (any* f . xs)
  (and (not (null? xs))
       (or (f (car xs))
           (apply any* f (cdr xs)))))

(define (opt-arg argv i . default)
  (if (<= (length argv)
          i)
    (and (not (null? default))
         (car default))
    (list-ref argv i)))

(define keyword-arg
  (case-lambda
    ((args key default)
     (let ((m (memv key args)))
      (if (and m (not (null? (cdr m))))
        (cadr m)
        default)))
    ((args key)
     (keyword-arg args key #f))))

(define const
  (case-lambda
    ((c no-args)
     (if no-args
       (lambda () c)
       (lambda (x . xs) c)))
    ((c)
     (lambda (x . xs) c))))

(define (alist? lst)
  (and (pair? lst)
       (all (conj pair?
                  (comp symbol? car))
            lst)))

(define (assv-get key alist)
  (and-let* ((a (assv key alist)))
            (cdr a)))

(define (assv-set key value alist)
  (let loop ((head '())
             (tail alist))
    (if (null? tail)
      (cons (cons key value) alist)
      (if (eqv? (caar tail) key)
        (cons-list (cons (cons key value)
                         (cdr tail))
                   head)
        (loop (cons (car tail) head)
              (cdr tail))))))

(define get
  (case-lambda
    ((keys alist default)
     (cond
       ((symbol? keys)
        (or (assv-get keys alist)
            default))
       ((list? keys)
        (let ((x (let ((x (assv-get (if (pair? (car keys))
                                      (caar keys)
                                      (car keys))
                                    alist)))
                   (if (pair? (car keys))
                     (list-ref x
                               (let ((idx (cadar keys)))
                                (if (< idx 0)
                                  (+ (length x) idx)
                                  idx)))
                     x))))
          (if (null? (cdr keys))
            x
            (get (cdr keys) x))))
       (else default)))
    ((keys alist)
     (get keys alist #f))))

(define (set keys value alist)
  (if (symbol? keys)
    (assv-set keys value alist)
    (let* ((key (if (pair? (car keys))
                  (caar keys)
                  (car keys)))
           (x (assv-get key alist))
           (idx (and (pair? (car keys))
                     (let ((idx (cadar keys)))
                      (if (< idx 0)
                        (+ (length x) idx)
                        idx)))))
      (assv-set key
                (if (pair? (car keys))
                  (replace-ref x
                               idx
                               (if (null? (cdr keys))
                                 value
                                 (set (cdr keys)
                                      value
                                      (list-ref x idx))))
                  (if (null? (cdr keys))
                    value
                    (set (cdr keys) value x)))
                alist))))

(define (del keys alist)
  (if (or (symbol? keys)
          (null? (cdr keys)))
    (filter (! (comp (cut eqv? (unlist keys) <>)
                   car))
            alist)
    (set (but-last keys)
         (del (last-car keys)
              (get (but-last keys)
                   alist))
         alist)))

(define (rename keys new-key alist)
  (if (or (symbol? keys)
          (null? (cdr keys)))
    (let* ((key (unlist keys))
           (x (get key alist)))
      (if x
        (set key
             x
             (del key alist))
        alist))
    (let ((table (get (but-last keys) alist))
          (x (get keys alist)))
      (if x
        (set (but-last keys)
             (set new-key
                  x
                  (del (last-car keys)
                       table))
             alist)
        alist))))

(define (push keys value alist)
  (set keys
       (cons value
             (or (get keys alist)
                 '()))
       alist))

(define (push-back keys value alist)
  (set keys
       (append (or (get keys alist)
                   '())
               (list value))
       alist))

(define (push-new keys value alist)
  (set keys
       (unique (cons value
                     (get keys alist)))
       alist))

(define (pop keys alist)
  (set keys
       (cdr (get keys alist))
       alist))

(define (update alist new)
  (foldr (lambda (x alist)
           (let+ (key . value) x
             (if (alist? value)
               (set key
                    (update (get key alist)
                            value)
                    alist)
               (set key value alist))))
         alist
         new))

(define getf
  (case-lambda
    ((k lst d)
     (let ((x (memv k lst)))
      (if x (cadr x) d)))
    ((k lst)
     (getf k lst #f))))

(define (find-file file dirs)
  (if (null? dirs)
    #f
    (let ((path (concat (car dirs) "/" file)))
     (if (file-exists? path)
       path
       (find-file file (cdr dirs))))))

(define (>>= m f)
  (lambda (v)
    (call-with-values
      (lambda () (m v))
      (lambda (nx nv)
        ((f nx) nv)))))

(define (return . args)
  (lambda (v)
    (values (cond ((null? args)
                   (void))
                  ((null? (cdr args))
                   (car args))
                  (else args))
            v)))

(define (get-state)
  (lambda (state)
    (values state state)))

(define (put-state x)
  (lambda (state)
    (values x x)))

(define (mod-state f)
  (lambda (state)
    (let ((state (f state)))
     (values state state))))

(define (set-state key value)
  (mod-state (cut set key value <>)))

(define (push-state key value)
  (>>=
    (get-state)
    (lambda (state)
      (put-state (set key
                      (append (get key state)
                              (list value))
                      state)))))

(define inc-state
  (opt-lambda (key & (by 1))
    (>>=
      (get-state)
      (lambda (state)
        (put-state (set key
                        (+ (get key state) by)
                        state))))))

(define (eval-state m state)
  (call-with-values
    (lambda () (m state))
    (lambda (_ state) state)))

(define (lift-m f)
  (lambda (x)
    (lambda (v)
      (values (f x) v))))

(define (map-m f x . xs)
  (letrec
    ((loop
       (lambda (xs result)
         (if (any null? xs)
           (return (reverse result))
           (do
             (x <- (apply f (map car xs)))
             (loop (map cdr xs)
                   (cons x result)))))))
    (loop (cons x xs)
          '())))

(define (fold-m m x xs)
  (if (null? xs)
    (return x)
    (do
      (a <- (m x (car xs)))
      (fold-m m a (cdr xs)))))

(define (for-each-m m xs)
  (letrec
    ((loop
       (lambda (xs)
         (if (null? xs)
           (return)
           (do
             (m (car xs))
             (loop (cdr xs)))))))
    (loop xs)))

(define (if-m c t f)
  (do
    (res <- c)
    (if res t f)))

(define (dbgfmt opt s . args)
  (do
    ((: options) <- (get-state))
    (if (get opt options)
      (return (apply fmt
                     (current-error-port)
                     (concat s "\n")
                     args))
      (return))))

(define empty-string?
  (comp zero? string-length))

(define (concat-list lst)
  (foldl string-append
         ""
         lst))

(define (join-str del xs)
  (or (foldr (lambda (a b)
               (if b
                 (string-append a del b)
                 a))
             #f
             xs)
      ""))

(define (concat x . xs)
  (concat-list (map repr (cons x xs))))

(define ljust
  (opt-lambda (str n & (ch #\space))
    (string-append
      str
      (make-string (max (- n
                           (string-length str))
                        0)
                   ch))))

(define rjust
  (opt-lambda (str n & (ch #\space))
    (string-append
      (make-string (max (- n
                           (string-length str))
                        0)
                   ch)
      str)))

(define (lstrip str char)
  (let loop ((len (string-length str))
             (i 0))
    (cond ((>= i len) "")
          ((char=? (string-ref str i)
                   char)
           (loop len (+ i 1)))
          (else (substr str i len)))))

(define (rstrip str char)
  (let loop ((i (string-length str)))
   (cond ((<= i 0) "")
         ((char=? (string-ref str (- i 1))
                  char)
          (loop (- i 1)))
         (else (substr str 0 i)))))

(define (strip str char)
  (lstrip (rstrip str char) char))

(define (makesym . args)
  (define (loop res tail)
    (if (null? tail)
      res
      (loop (string-append res (repr (car tail)))
            (cdr tail))))
  (string->symbol (loop "" args)))

(define subsym
  (opt-lambda (sym start & end)
    (let ((s (symbol->string sym)))
     (string->symbol
       (substr s start (or end (string-length s)))))))

(define (reverse-string str)
  (list->string (reverse (string->list str))))

(define string-search
  (opt-lambda (needle haystack & (from 0) rev)
    (if rev
      (let ((pos (string-search (reverse-string needle)
                                (reverse-string haystack)
                                from)))
        (and pos
             (- (string-length haystack)
                (+ (string-length needle)
                   pos))))
      (let loop ((start from)
                 (len (string-length needle))
                 (h-len (string-length haystack)))
        (if (> (+ start len) h-len)
          #f
          (or (let inner ((i 0))
               (cond
                 ((>= i len) start)
                 ((char=? (string-ref needle i)
                          (string-ref haystack
                                      (+ i start)))
                  (inner (+ i 1)))
                 (else #f)))
              (loop (+ start 1)
                    len
                    h-len)))))))

(define (starts-with with what)
  (or (string=? with what)
      (and (>= (string-length what)
               (string-length with))
           (string=? (substr what 0 (string-length with))
                     with))))

(define (ends-with with what)
  (and (>= (string-length what)
           (string-length with))
       (string=? (substr what
                            (- (string-length what)
                               (string-length with)))
                 with)))

(define (str-replace what with str)
  (let loop ((i 0)
             (result '()))
    (let ((pos (string-search what str i)))
     (if pos
       (loop (+ pos (string-length what))
             (cons* with
                    (substr str i pos)
                    result))
       (concat-list
         (reverse
           (cons (substr str i (string-length str))
                 result)))))))

(define (translate-str dict str)
  (if (null? dict)
    str
    (let+ ((what with) . rest) dict
      (translate-str rest
                     (str-replace what with str)))))

(define (symbol-replace what with sym)
  (string->symbol (str-replace what
                               with
                               (symbol->string sym))))

(define split-str
  (opt-lambda (del s & max)
    (let loop ((r '())
               (i 0)
               (m max)
               (len (string-length del)))
      (let ((pos (string-search del s i)))
       (if (or (not pos)
               (and m (= m 0)))
         (reverse (cons (substr s
                                   i
                                   (string-length s))
                        r))
         (loop (cons (substr s i pos)
                     r)
               (+ pos len)
               (and m (- m 1))
               len))))))

(define (file-base-name f)
  (last-car (split-str "/" f)))

(define (split-path s)
  (let ((path (filter (! empty-string?)
                      (split-str "/" s))))
    (if (char=? (string-ref s 0) #\/)
      (cons (concat "/" (car path))
            (cdr path))
      (cons "." path))))

(define (join-path p)
  (join-str "/" p))

(define (normalize-path s)
  (-> (split-path s)
    join-path))

(define (split-dir s)
  (let ((path (split-path s)))
   (list (join-str "/" (but-last path))
         (last-car path))))

(define (file-directory s)
  (-> s split-dir car))

(define (split-file-suffix filename)
  (let ((pos (string-search "." filename 0 #t)))
   (if (and pos
            (not (string-search "/" filename pos)))
     (list (substr filename 0 pos)
           (substr filename
                      (+ pos 1)
                      (string-length filename)))
     (list filename ""))))

(define (drop-file-suffix f)
  (-> f split-file-suffix car))

(define (file-suffix f)
  (-> f split-file-suffix cadr))

(define (set-file-suffix f e)
  (-> f
    drop-file-suffix
    (concat <> e)))

(define (string->int s radix)
  (define (ascii->int x)
    (let ((i (cond ((and (>= x 48)
                         (<= x 57))
                    (- x 48))
                   ((and (>= x 65)
                         (<= x 90))
                    (- x 55))
                   (else #f))))
      (and (< i radix) i)))
  (-> s
    string->list
    (map (comp ascii->int
             char->integer
             char-upcase)
         <>)
    reverse
    enumerate
    (foldr (lambda (x r)
             (let+ (i x) x
               (and r x (+ r (* x (expt radix i))))))
           0
           <>)))

(define (get-executable-path name)
  (cond ((eqv? (string-ref name 0) #\/)
         name)
        ((in? #\/ (string->list name))
         (string-append (current-directory)
                        (string-append "/" name)))
        (else
          (find-file
            name
            (split-str ":" (get-env-variable "PATH"))))))

