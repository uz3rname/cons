(define-syntax and-let*
  (syntax-rules
    ()
    ((_ ((name form)) body ...)
     (let ((name form))
      (if name
        (begin body ...)
        #f)))  
    ((_ ((name form) rest ...) body ...)
     (let ((name form))
      (if name
        (and-let* (rest ...) body ...)
        #f)))))

(define-syntax %fcase
  (syntax-rules (else)
    ((_ key (else body ...))
     (begin body ...))
    ((_ key ((f) body ...))
     (if (f key)
       (begin body ...)))
    ((_ key ((f fs ...) body ...))
     (if (f key)
       (begin body ...)
       (%fcase key ((fs ...) body ...))))
    ((_ key ((f) body ...) rest ...)
     (if (f key)
       (begin body ...)
       (%fcase key rest ...)))
    ((_ key ((f fs ...) body ...) rest ...)
     (if (f key)
       (begin body ...)
       (%fcase key ((fs ...) body ...) rest ...)))))

(define-syntax fcase
  (syntax-rules ()
    ((_ key clause ...)
     (let ((x key))
      (%fcase x clause ...)))))

(define-syntax scase
  (syntax-rules (else)
    ((_ key (else body ...))
     (begin body ...))
    ((_ key ((x) body ...))
     (let ((k key))
      (if (equal? x k)
        (begin body ...))))
    ((_ key ((x xs ...) body ...))
     (let ((k key))
      (if (equal? x k)
        (begin body ...)
        (scase k ((xs ...) body ...)))))
    ((_ key ((x) body ...) rest ...)
     (let ((k key))
      (if (equal? x k)
        (begin body ...)
        (scase k rest ...))))
    ((_ key ((x xs ...) body ...) rest ...)
     (let ((k key))
      (if (equal? x k)
        (begin body ...)
        (scase k ((xs ...) body ...) rest ...))))))

(define-syntax vlambda
  (syntax-rules ()
    ((_ xs body ...)
     ((lambda ()
        (define (fn . xs)
          body ...)
        fn)))))

(define (v-arg-lambda f . cs)
  (define (%lambda . args)
    (if (null? cs)
      (apply f args)
      (apply f (car cs) args)))
  %lambda)

(define-syntax cut
  (syntax-rules (<>)
    ((_ f)
     (lambda (x . xs)
       (apply f x xs)))
    ((_ f <>)
     (v-arg-lambda f))
    ((_ f c)
     (v-arg-lambda f c))
    ((_ f <> cs ...)
     (lambda (x . xs)
       (apply (cut f x cs ...)
              xs)))
    ((_ f c cs ...)
     (cut (cut f c) cs ...))))

(define-syntax ->
  (syntax-rules (lambda)
    ((_ x (lambda args ...))
     ((lambda args ...) x))
    ((_ x (f args ...))
     ((cut f args ...) x))
    ((_ x f)
     (f x))
    ((_ x (lambda args ...) fs ...)
     (-> ((lambda args ...) x) fs ...))
    ((_ x (f args ...) fs ...)
     (-> ((cut f args ...) x) fs ...))
    ((_ x f fs ...)
     (-> (f x) fs ...))))

(define-syntax when
  (syntax-rules ()
    ((_ cnd body ...)
     (if cnd
       (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ cnd body ...)
     (if (not cnd)
       (begin body ...)))))

(define-syntax %let-assoc
  (syntax-rules ()
    ((_ ((as key)) expr body ...)
     (let ((as (get 'key expr)))
      body ...))
    ((_ (key) expr body ...)
     (let ((key (get 'key expr)))
      body ...))
    ((_ (key keys ...) expr body ...)
     (%let-assoc (key) expr
                 (%let-assoc (keys ...) expr
                             body ...)))))

(define-syntax let-assoc
  (syntax-rules ()
    ((_ args ...)
     (%let-assoc args ...))))

(define-syntax %let+
  (syntax-rules (:)
    ((_ (: . xs) expr body ...)
     (let-assoc xs expr body ...))
    ((_ ((xs ...)) expr body ...)
     (%let+ (xs ...) (car expr) body ...))
    ((_ ((x . xs)) expr body ...)
     (%let+ (x . xs) (car expr) body ...))
    ((_ ((x . xs) rest ...) expr body ...)
     (%let+ (x . xs) (car expr)
            (%let+ (rest ...) (cdr expr) body ...)))
    ((_ ((xs ...) . rest) expr body ...)
     (%let+ (xs ...) (car expr)
            (let ((rest (cdr expr))) body ...)))  
    ((_ ((xs ...) rest ...) expr body ...)
     (%let+ (xs ...) (car expr)
            (%let+ (rest ...) (cdr expr) body ...)))  
    ((_ (x) expr body ...)
     (let ((x (car expr)))
      (begin body ...)))
    ((_ (x . xs) expr body ...)
     (let ((x (car expr)))
      (%let+ xs (cdr expr)
             (begin body ...))))
    ((_ x expr body ...)
     (let ((x expr))
      (begin body ...)))))

(define-syntax let+
  (syntax-rules ()
    ((_ arg expr body ...)
     (let ((x expr))
      (%let+ arg x body ...)))))

(define-syntax table
  (syntax-rules ()
    ((_ (key value) ...)
     (map cons
          (list (quote key) ...)
          (list value ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((_ (name ...) expr body ...)
     (call-with-values
       (lambda () expr)
       (lambda (name ...) body ...)))))

(define-syntax do
  (syntax-rules (<-)
    ((_ m)
     (lambda (v) (m v)))
    ((_ ((x ...) <- m) ms ...)
     (>>=
       m
       (lambda (xx)
         (let+ (x ...) xx
           (do ms ...)))))
    ((_ ((x . xs) <- m) ms ...)
     (>>=
       m
       (lambda (xx)
         (let+ (x . xs) xx
           (do ms ...)))))
    ((_ (x <- m) ms ...)
     (>>=
       m
       (lambda (x)
         (do ms ...))))  
    ((_ m ms ...)
     (>>=
       m
       (lambda (_)
         (do ms ...))))))

(define-syntax when-m
  (syntax-rules ()
    ((_ cnd body ...)
     (if cnd
       (do body ...)
       (return)))))

(define-syntax unless-m
  (syntax-rules ()
    ((_ cnd body ...)
     (if cnd
       (return)
       (do body ...)))))

(define-syntax assert
  (syntax-rules ()
    ((_ x err ...)
     (or x (throw err ...)))))

(define-syntax accept
  (syntax-rules ()
    ((_ p x err ...)
     (or (and (p x) x)
         (throw err ...)))))

(define-syntax !
  (syntax-rules ()
    ((_ f)
     (lambda (x . xs)
       (not (apply f x xs))))))

(define-syntax comp
  (syntax-rules ()
    ((_ f)
     f)
    ((_ f fs ...)
     (lambda (x . xs)
       (f (apply (comp fs ...)
                 x
                 xs))))))

(define-syntax conj%
  (syntax-rules ()
    ((_ x f ...)
     (and (f x) ...))))

(define-syntax conj
  (syntax-rules ()
    ((_ fs ...)
     (lambda (x)
       (conj% x fs ...)))))

(define-syntax disj%
  (syntax-rules ()
    ((_ x f ...)
     (or (f x) ...))))

(define-syntax disj
  (syntax-rules ()
    ((_ fs ...)
     (lambda (x)
       (disj% x fs ...)))))

(define-syntax case-lambda%
  (syntax-rules ()
    ((_ l xs (args body ...))
     (if (= l (length 'args))
       (let+ args xs body ...)
       (throw 'error "Invalid argument count.")))
    ((_ l xs (args body ...) rest ...)
     (if (= l (length 'args))
       (let+ args xs body ...)
       (case-lambda% l xs rest ...)))))

(define-syntax case-lambda
  (syntax-rules ()
    ((_ clauses ...)
     (vlambda xs
       (let ((l (length xs)))
        (case-lambda% l xs clauses ...))))))

(define-syntax opt-lambda%%
  (syntax-rules ()
    ((_ i rest () body ...)
     (begin body ...))
    ((_ i rest ((x d) xs ...) body ...)
     (let ((x (opt-arg rest i d)))
      (opt-lambda%%
        (+ i 1)
        rest
        (xs ...)
        body ...)))
    ((_ i rest (x xs ...) body ...)
     (let ((x (opt-arg rest i #f)))
      (opt-lambda%%
        (+ i 1)
        rest
        (xs ...)
        body ...)))))

(define-syntax opt-lambda%
  (syntax-rules (&)
    ((_ args () body ...)
     (lambda args body ...))
    ((_ (args ...) (& xs ...) body ...)
     (lambda (args ... . rest)
       (opt-lambda%% 0 rest (xs ...) body ...)))
    ((_ () (x xs ...) body ...)
     (opt-lambda%
       (x)
       (xs ...)
       body ...))
    ((_ (args ...) (x xs ...) body ...)
     (opt-lambda%
       (args ... x)
       (xs ...)
       body ...))))

(define-syntax opt-lambda
  (syntax-rules ()
    ((_ args body ...)
     (opt-lambda% () args body ...))))

(define-syntax kw-lambda%%
  (syntax-rules ()
    ((_ rest () body ...)
     (begin body ...))
    ((_ rest ((k v) xs ...) body ...)
     (let ((k (getf 'k rest v)))
      (kw-lambda%% rest (xs ...) body ...)))
    ((_ rest (k xs ...) body ...)
     (let ((k (getf 'k rest)))
      (kw-lambda%% rest (xs ...) body ...)))))

(define-syntax kw-lambda%
  (syntax-rules (&)
    ((_ args () body ...)
     (lambda args body ...))
    ((_ (args ...) (& xs ...) body ...)
     (lambda (args ... . rest)
       (kw-lambda%% rest (xs ...) body ...)))
    ((_ () (x xs ...) body ...)
     (kw-lambda%
       (x)
       (xs ...)
       body ...))
    ((_ (args ...) (x xs ...) body ...)
     (kw-lambda%
       (args ... x)
       (xs ...)
       body ...))))

(define-syntax kw-lambda
  (syntax-rules ()
    ((_ args body ...)
     (kw-lambda% () args body ...))))

