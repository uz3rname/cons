;-unit lex -uses prelude
(include "inc/macro.scm")

(define (char-decimal? ch)
  (or (char-numeric? ch)
      (memv ch '(#\+ #\- #\.))))

(define (symbol-char? ch)
  (not (in? ch '(#\; #\( #\) #\[ #\] #\#
                 #\" #\\ #\space #\newline #\tab #\return))))

(define (get-char . args)
  (do
    ((: str pos len) <- (get-state))
    (if (< pos len)
      (return (string-ref str
                          (+ pos (opt-arg args 0 0))))
      (return #f))))

(define (next-char)
  (do
    ((: str pos line col) <- (get-state))
    (set-state 'pos (+ pos 1))
    (ch <- (get-char))
    (if ch
      (if (eqv? ch #\newline)
        (do
          (set-state 'line (+ line 1))
          (set-state 'col 1)
          (return ch))
        (do
          (set-state 'col (+ col 1))
          (return ch)))
      (return #f))))

(define (terminal-token tag)
  (do
    ((: str pos line col) <- (get-state))
    (next-char)
    (return (list tag #t line col))))

(define (lexical-error msg)
  (do
    ((: line col) <- (get-state))
    (throw (make-error 'lexical-error
                       "{} (line: {}, col: {})"
                       msg
                       line
                       col))))

(define (take-chars-while tag f)
  (do
    ((: pos line col) <- (get-state))
    (set-state 'start pos)
    (letrec
      ((loop (lambda ()
               (do
                 (ch <- (get-char))
                 ((: str start pos) <- (get-state))
                 (if (or (not ch)
                         (not (f ch)))
                   (return (list tag
                                 (substr str start pos)
                                 line
                                 col))     
                   (do
                     (next-char)
                     (loop)))))))
      (loop))))

(define (get-symbol)
  (take-chars-while 'symbol symbol-char?))

(define (get-number)
  (take-chars-while 'number char-decimal?))

(define (take-n-chars n)
  (define (loop n r)
    (if (= n 0)
      (return
        (list->string (reverse r)))
      (do
        (ch <- (get-char))
        (next-char)
        (loop (- n 1) (cons ch r)))))
  (loop n '()))

(define (take-chars f)
  (define (loop r)
    (do
      (ch <- (get-char))
      (if (and ch (f ch))
        (do
          (next-char)
          (loop (cons ch r)))
        (return
          (list->string (reverse r))))))
  (loop '()))

(define (char-digit? ch b)
  (let ((i (char->integer ch)))
   (and (>= i 48)
        (or (and (<= b 10)
                 (< i (+ 48 b)))
            (and (> b 10)
                 (or (<= i 57)
                     (and (>= i 65)
                          (< i (+ 65 (- b 10))))))))))

(define (escaped-char)
  (define (take-num-repr base)
    (do
      (s <- (take-chars
              (cut char-digit? <> base)))
      (return
        (integer->char
          (string->int s base)))))
  (do
    (ch <- (get-char))
    (next-char)
    (case ch
      ((#\n)
       (return #\newline))
      ((#\t)
       (return #\tab))
      ((#\x)
       (take-num-repr 16))
      ((#\0)
       (take-num-repr 8))
      (else (return ch)))))

(define (get-string)
  (do
    (next-char)
    ((: pos line col) <- (get-state))
    (set-state 'token '())
    (letrec
      ((loop (lambda ()
               (do
                 (ch <- (get-char))
                 ((: str token pos) <- (get-state))
                 (case ch
                   ((#\\ )
                    (do
                      (next-char)
                      (ch <- (escaped-char))
                      (set-state 'token
                                 (cons ch token))
                      (loop)))
                   ((#\" )
                    (do
                      (next-char)
                      (return (list 'string
                                    (list->string
                                      (reverse token))
                                    line
                                    col))))
                   (else
                     (do
                       (next-char)
                       (set-state 'token (cons ch token))
                       (loop))))))))
      (loop))))

(define (skip-line)
  (letrec
    ((loop (lambda (res)
             (do
               (ch <- (get-char))
               (if ch
                 (if (char=? ch #\newline)
                   (return (list->string (reverse res)))
                   (do
                     (next-char)
                     (loop (cons ch res))))
                 (return #f))))))
    (loop '())))

(define (preproc-macro? x)
  (and (> (string-length x) 0)
       (char=? (string-ref x 0) #\#)))

(define (do-preproc-macro line)
  (define (loop tail)
    (if (null? tail)
      (return)
      (case (cadar tail)
        ((file:)
         (loop (cddr tail)))
        ((line:)
         (do
           (set-state 'line
                      (cadadr tail))
           (set-state 'col 1)
           (loop (cddr tail))))
        (else
          (loop (cdr tail))))))
  (loop (tokenize (substr line 1))))

(define (do-hash)
  (do
    ((: line col) <- (get-state))
    (if (and (= line 1)
             (= col 1))
      (do
        (skip-line)
        (get-token))
      (do
        (ch <- (get-char 1))
        (case ch
          ((#\\ )
           (do
             (next-char)
             (next-char)
             (ch <- (get-char))
             ((: line col) <- (get-state))
             (cond ((not ch)
                    (lexical-error "Unexpected EOF in character representation"))
                   ((or (char-alphabetic? ch)
                        (char-numeric? ch))
                    (>>=
                      (take-chars (disj char-alphabetic?
                                        char-numeric?))
                      (lambda (s)
                        (do
                          ;(next-char)
                          (let ((ch (char-value s)))
                           (if ch
                             (return
                               (list 'char ch line col))
                             (lexical-error "Invalid character representation")))))))
                   (else
                     (do
                       (next-char)
                       (return (list 'char ch line col)))))))
          (else (terminal-token 'hash))))))) 

(define (get-token)
  (do
    (ch <- (get-char))
    (case ch
      ((#f)
       (return #f))
      ((#\; )
       (do
         (next-char)
         (line <- (skip-line))
         (if (preproc-macro? line)
           (do-preproc-macro line)
           (return))
         (get-token)))
      ((#\space #\newline)
       (do
         (next-char)
         (get-token)))
      ((#\( )
       (terminal-token 'open-par))
      ((#\) )
       (terminal-token 'close-par))
      ((#\[)
       (terminal-token 'open-br))
      ((#\] )
       (terminal-token 'close-br))
      ((#\# )
       (do-hash))
      ((#\' )
       (terminal-token 'quote))
      ((#\" )
       (get-string))
      (else
        (cond ((char-numeric? ch)
               (case ch
                 ((#\0)
                  (do
                    (next <- (get-char 1))
                    (if (char-numeric? next)
                      (get-symbol)
                      (get-number))))
                 (else (get-number))))
              ((memv ch '(#\+ #\-))
               (do
                 (next <- (get-char 1))
                 (if (char-numeric? next)
                   (get-number)
                   (get-symbol))))
              (else
                (get-symbol)))))))

(define (char-value s)
  (if (= (string-length s) 1)
    (string-ref s 0)
    (scase s
      (("nl") #\newline)
      (("return") #\return)
      (("space") #\space)
      (("tab") #\tab)
      (else
        (cond ((and (char=? (string-ref s 0)
                            #\0)
                    (all char-numeric?
                         (cdr (string->list s))))
               (integer->char
                 (string->int (substr s 1) 8)))
              ((and (char=? (string-ref s 0)
                            #\x)
                    (all (disj char-numeric?
                               (cut in? <> '(#\a #\b #\c #\d #\e #\f)))
                         (cdr (string->list s))))
               (integer->char
                 (string->int (substr s 1) 16))))))))

(define (tokenizer)
  (letrec
    ((loop (lambda ()
             (do
               (tok <- (get-token))
               ((: result) <- (get-state))
               (if tok
                 (do
                   (set-state 'result
                              (cons tok result))
                   (loop))
                 (return (reverse result)))))))
    (loop)))

(define (tokenizer-state str)
  (table (str str)
         (pos 0)
         (line 1)
         (col 1)
         (len (string-length str))
         (result '())))

(define (tokenize str)
  (let-values (e _) ((tokenizer)
                     (tokenizer-state str))
    e))
