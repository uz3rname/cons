;-unit codegen -uses prelude -uses types
(include "inc/macro.scm")

(define (get-comp-opt x)
  (do
    ((: opts) <- (get-state))
    (return (get x opts))))

(define (all-regs)
  '(rax rbx rcx rdx rdi rsi rbp rsp
    eax ebx ecx edx edi esi ebp esp
    ax bx cx dx di si bp sp
    al bl cl dl sil dil bpl spl
    r8 r9 r10 r11 r12 r13 r14 r15
    r8d r9d r10d r11d r12d r13d r14d r15d
    r8w r9w r10w r11w r12w r13w r14w r15w
    r8b r9b r10b r11b r12b r13b r14b r15b
    rip))

(define (closure-reg arch)
  (case arch
    ((amd64) 'r10)
    ((x86) 'ebx)))

(define (tmp-reg arch)
  (case arch
    ((amd64) 'r11)
    ((x86) 'ecx)))

(define (base-reg arch)
  (case arch
    ((amd64) 'rbp)
    ((x86) 'ebp)))

(define (register? x)
  (in? x (all-regs)))

(define (xmm-register? x)
  (and (symbol? x)
       (starts-with "xmm" (symbol->string x))))

(define (xmm-register x)
  (makesym "xmm" x))

(define (operand-base x)
  (and (pair? x)
       (cond ((null? (cdr x))
              (car x))
             ((in? (car x) '(+ -))
              (cadr x))
             (else #f))))

(define (operand-offset x)
  (and (pair? x)
       (cond ((null? (cdr x))
              0)
             ((in? (car x) '(+ -))
              (caddr x))
             ((eqv? (car x) '$)
              0)
             (else #f))))

(define (operand-modifier x)
  (and (pair? x)
       (if (null? (cdr x))
         '+
         (car x))))

(define (memory? v)
  (or (pair? v)
      (and (symbol? v)
           (not (register? v))
           (not (xmm-register? v)))))

(define (register-idx x)
  (and (register? x)
       (case x
         ((al ax eax rax) 0)
         ((bl bx ebx rbx) 1)
         ((cl cx ecx rcx) 2)
         ((dl dx edx rdx) 3)
         ((dil di edi rdi) 4)
         ((sil si esi rsi) 5)
         ((bpl bp ebp rbp) 6)
         ((spl sp esp rsp) 7)
         (else
           (string->number
             (apply
               concat
               (take-while char-numeric?
                           (cdr (string->list
                                  (symbol->string x))))))))))

(define (register=? a b)
  (and (register? a)
       (register? b)
       (= (register-idx a)
          (register-idx b))))

(define (operand=? a b)
  (equal? a b))

(define (operand-size x)
  (if (register? x)
    (case x
      ((rax rbx rcx rdx rdi rsi rbp rsp r8 r9 r10 r11 r12 r13 r14 r15)
       'quad)
      ((eax ebx ecx edx edi esi ebp esp r8d r9d r10d r11d r12d r13d r14d r15d)
       'long)
      ((ax bx cx dx di si bp sp r8w r9w r10w r11w r12w r13w r14w r15w)
       'word)
      ((al bl cl dl sil dil bpl spl r8b r9b r10b r11b r12b r13b r14b r15b)
       'byte))
    'quad))

(define (size-instruction op size)
  (makesym op
           (case size
             ((quad) "q")
             ((long) "l")
             ((word) "w")
             ((byte) "b")
             (else (throw 'error "Unknown operand size ({})" size)))))

(define (unsize-instruction op)
  (define (match-ops a b)
    (and (= (string-length a)
            (string-length b))
         (all (lambda (x y)
                (or (and (char=? x #\1)
                         (in? y '(#\q #\w #\l #\b)))
                    (and (char=? x #\2)
                         (in? y '(#\d #\s #\w #\b)))
                    (char=? x y)))
              (string->list a)
              (string->list b))))
  (define (i-list)
    '("mov1" "add1" "sub1" "imul1" "idiv1" "push1" "pop1"
      "movs2" "adds2" "subs2" "muls2" "divs2" "ucomis2"))
  (let* ((s (symbol->string op))
         (i (find (conj (cut match-ops <> s)
                        (! (cut string=? <> s)))
                  (i-list))))
    (if i
      (let ((l (- (string-length i) 1)))
       (list (makesym (substr i 0 l))
             (case (string-ref s l)
               ((#\q #\d) 'quad)
               ((#\l #\s) 'long)
               ((#\w) 'word)
               ((#\b) 'byte))))
      (list op #f))))

(define (sse-instruction op size)
  (makesym op
           (case size
             ((quad) "sd")
             ((long) "ss")
             ((word) "sw")
             ((byte) "sb")
             (else (throw 'error "Unknown operand size ({})" size)))))

(define (sse-arithm-instruction op size)
  (makesym
    (case op
      ((+) "adds")
      ((-) "subs")
      ((*) "muls")
      ((/) "divs"))
    (case size
      ((quad) "d")
      ((long) "s")
      ((word) "w")
      ((byte) "b"))))

(define (size-location loc size)
  (let ((x loc))
   (if (register? x)
     (case x
       ((rax rbx rcx rdx rdi rsi rsp rbp)
        (case size
          ((quad) loc)
          ((long) (makesym "e" (subsym x 1)))
          ((word) (subsym x 1))
          ((byte)
           (case x
             ((rax rbx rcx rdx)
              (makesym (subsym x 1 2) "l"))
             ((rdi rsi rsp rbp)
              (makesym (subsym x 1) "l"))))))
       ((r8 r9 r10 r11 r12 r13 r14 r15)
        (makesym x (case size
                     ((quad) "")
                     ((long) "d")
                     ((word) "w")
                     ((byte) "b"))))
       (else loc))
     loc)))

(define (int-arg-registers)
  '(rdi rsi rdx rcx r8 r9))

(define (scratch-regs arch)
  (case arch
    ((amd64)
     '(rbx r12 r13 r14 r15))
    ((x86)
     '(edx edi esi))))

(define (codegen-error e msg . rest)
  (make-error 'codegen-error
              "{} (file: {}, line: {}, col: {})"
              (apply fmt #f msg rest)
              (get 'file e)
              (get 'line e)
              (get 'col e)))

(define (emit x . xs)
  (for-each-m (lambda (x)
                (if (pair? x)
                  (emit-ins x)
                  (do
                    ((: code) <- (get-state))
                    (set-state 'code
                               (cons x code)))))
              (cons x xs)))

(define (emit-ins x)
  (let+ (op . args) x
    (let+ (s-op sz) (unsize-instruction op)
      (if (and (not (null? args))
               (in? s-op
                    '(mov add sub mul div imul idiv lea xor ucomi cmp push pop)))
        (do
          ((: arch) <- (get-state))
          (let* ((sz (or sz
                         (cond ((null? (cdr args))
                                (operand-size (car args)))
                               ((or (register? (last-car args))
                                    (all memory? args))
                                (operand-size (last-car args)))
                               ((any register? args)
                                (operand-size (find register? args)))
                               (else 'quad))))
                 (args (map (cut size-location <> sz)
                            args))
                 (tmp-reg (size-location (tmp-reg arch) sz)))
            (define (emit x . xs)
              (do
                ((: code) <- (get-state))
                (set-state 'code
                           (cons
                             (cons (size-instruction x sz)
                                   xs)
                             code))))
            (define (emit-sse x . xs)
              (do
                ((: code) <- (get-state))
                (set-state 'code
                           (cons
                             (cons (sse-instruction x sz)
                                   xs)
                             code))))
            (cond
              ((and (>= (length args) 2)
                    (xmm-register? (cadr args))
                    (register? (car args)))
               (return))
              ((any xmm-register? args)
               (apply emit-sse s-op args))
              ((and (eqv? op 'imul)
                    (memory? (cadr args)))
               (do
                 (emit 'mov
                       (cadr args)
                       tmp-reg)
                 (emit 'imul
                       (car args)
                       tmp-reg)
                 (emit 'mov
                       tmp-reg
                       (cadr args))))
              ((and (not (null? (cdr args)))
                    (all memory? args))
               (do
                 (emit 'mov
                       (car args)
                       tmp-reg)
                 (emit s-op
                       tmp-reg
                       (cadr args))))
              (else
                (apply emit s-op args)))))
        (do
          ((: code) <- (get-state))
          (set-state 'code (cons x code)))))))

(define (leaf-expr? body)
  (null? (tree-search (lambda (x)
                        (and (alist? x)
                             (let+ (: expr) x
                               (or (memv expr
                                         '(call tail-call struct list array c-array closure))
                                   (and (eqv? expr 'ref)
                                        (eqv? (get 'type x) 'function))))))
                      (list body))))

(define (compile-const e trgt)
  (let+ (: type value) e
    (case type
      ((int)
       (return value))
      ((real)
       (do
         (lab <- (find-data 'double (list value)))
         (if lab
           (compile-symbol-value-ref lab trgt)
           (do
             (lab <- (make-data (list (list 'double value))))
             (compile-symbol-value-ref lab trgt)))))
      (else (throw 'not-implemented "")))))

(define (find-data type value)
  (define (loop rest)
    (if (null? rest)
      (return #f)
      (let+ (name x . xs) (car rest)
        (if (and (eqv? type (car x))
                 (equal? value (cdr x)))
          (return name)
          (loop (cdr rest))))))
  (do
    ((: data) <- (get-state))
    (loop data)))

(define (make-data xs)
  (do
    ((: data) <- (get-state))
    (let ((lab (makesym ".LC" (length data))))
      (do
        (set-state `(data ,lab) xs)
        (return lab)))))

(define (compile-sequence e trgt)
  (let+ (: sub) e
    (fold-m (lambda (_ x)
              (compile-expr x trgt))
            0
            sub)))

(define (compile-ref e trgt)
  (let+ (: type value arg-types lvl) e
    (case type
      ((arg)
       (do
         ((: arg-locations) <- (get-state))
         (return (list-ref arg-locations value))))
      ((function)
       (do
         (reg <- (get-int-reg))
         (compile-mem-alloc 8 reg)
         (loc <- (compile-symbol-ref value trgt))
         (emit `(mov ,loc (,reg))
               `(mov ,reg ,loc))
         (free-reg reg)
         (return loc)))
      ((function-ptr)
       (compile-symbol-ref value trgt))
      ((symbol)
       (compile-symbol-ref value trgt))
      ((symbol-value)
       (compile-symbol-value-ref value trgt))
      ((closure)
       (do
         ((: arch) <- (get-state))
         (return `(+ ,(closure-reg arch)
                     ,(+ 8 (* 8 value))))))
      (else (throw (codegen-error
                     e
                     "Unknown reference type ({})"
                     type))))))

(define (global-sym? s)
  (do
    ((: extern globals) <- (get-state))
    (return (or (in? s extern)
                (in? s globals)))))

(define (compile-symbol-ref s trgt)
  (do
    (pie <- (get-comp-opt 'opt-pie))
    ((: arch) <- (get-state))
    (if pie
      (do
        (reg <- (if (register? trgt)
                  (return trgt)
                  (return (tmp-reg arch))))
        (if-m (global-sym? s)
          (emit `(mov (GOTPCREL ,s)
                      ,reg))
          (emit `(lea (+ rip ,s)
                      ,reg)))
        (if (register? trgt)
          (return reg)
          (if (not trgt)
            (do
              (loc <- (new-location))
              (emit `(mov ,reg ,loc))
              (return loc))
            (do
              (emit `(mov ,reg ,trgt))
              (return trgt)))))
      (return `($ ,s)))))

(define (compile-symbol-value-ref s trgt)
  (do
    (pie <- (get-comp-opt 'opt-pie))
    ((: arch) <- (get-state))
    (if pie
      (do
        (reg <- (if (register? trgt)
                  (return trgt)
                  (return (tmp-reg arch))))
        (if-m (global-sym? s)
              (emit `(mov (GOTPCREL ,s)
                          ,reg))
              (emit `(lea (+ rip ,s)
                          ,reg)))
        (if (xmm-register? trgt)
          (do
            (emit `(mov (,reg) ,trgt))
            (return trgt))
          (do
            (emit `(mov (,reg) ,reg))
            (if (register? trgt)
              (return reg)
              (if trgt
                (do
                  (emit `(mov ,reg ,trgt))
                  (return trgt))
                (do
                  (loc <- (new-location))
                  (emit `(mov ,reg ,loc))
                  (return loc)))))))
      (return `(,s)))))

(define (compile-int-binary op e lh)
  (define (compile-div-op lh rh trgt)
    (do
      (rh
        <- (if (number? rh)
             (do
               (loc <- (new-location))
               (emit `(mov ,rh ,loc))
               (return loc))  
             (return rh)))
      (unless-m (eqv? lh 'rdx)
        (emit `(push rdx)))
      (if (eqv? rh 'rdx)
        (emit `(mov rdx r11)
              `(mov ,lh rax)
              `(cqo)
              `(idiv r11))
        (emit `(mov ,lh rax)
              `(cqo)
              `(idiv ,rh)))
      (unless-m (eqv? lh trgt)
        (emit `(mov ,trgt ,lh)))  
      (unless-m (eqv? lh 'rdx)
        (emit `(pop rdx)))))
  (do
    (rh <- (compile-expr e #f))
    (case op
      ((+ - * shr shl logand logor)
       (emit `(,(int-num-op op)
                ,rh
                ,lh)))
      ((/)
       (compile-div-op lh rh 'rax))
      ((mod)
       (compile-div-op lh rh 'rdx))
      (else (throw (codegen-error
                     e
                     "Unknown binary operator ({})"
                     op))))
    (free-reg rh)))

(define (reg-free? x)
  (do
    ((: free-int-regs) <- (get-state))
    (return (in? x free-int-regs))))

(define (int-num-op op)
  (case op
    ((+) 'add)
    ((-) 'sub)
    ((*) 'imul)
    ((shr) 'shr)
    ((shl) 'shl)
    ((logand) 'and)
    ((logor) 'or)
    ((xor) 'xor)
    (else (throw 'codegen-error
                 "Unknown binary operator ({})"
                 op))))  

(define (compile-int-num-expr e trgt)
  (let+ (: op args) e
    (do
      (lh <- (if (and (register? trgt)
                      #f ;TODO: fix
                      (or (not (= (register-idx trgt) 0))
                          (and (all leaf-expr? args)
                               (null? (tree-search
                                        (lambda (x)
                                          (and (alist? x)
                                               (eqv? (get 'expr x)
                                                     'int-num-expr)
                                               (in? (get 'op x)
                                                    '(/ mod))))
                                        (list e))))))
               (return trgt)
               (get-int-reg)))
      (compile-expr (car args) lh)
      (if (null? (cdr args))
        (when-m (eqv? op '-)
          (emit `(neg ,lh)))
        (for-each-m (cut compile-int-binary op <> lh)
                    (cdr args)))
;      (free-int-reg lh)
;      (ret <- (new-location))
;      (emit `(mov ,lh ,ret))
;      (return ret)   
      (if trgt
        (do
          (unless-m (operand=? lh trgt)
            (emit `(mov ,lh ,trgt))
            (free-reg lh))
          (return trgt))
        (if (register? lh)
          (do
            (loc <- (new-location))
            (emit `(mov ,lh ,loc))
            (free-reg lh)
            (return loc))
          (return lh))))))

(define (new-location)
  (do
    ((: stack-depth free-locations) <- (get-state))
    (if (null? free-locations)
      (do
        (inc-state 'stack-depth 8)
        (return `(- rbp ,(+ stack-depth 8))))
      (let ((loc (car free-locations)))
       (do
         (set-state 'free-locations
                    (filter (! (cut operand=? loc <>))
                            free-locations))
         (return loc))))))

(define (free-regs?)
  (do
    ((: free-int-regs) <- (get-state))
    (return (not (null? free-int-regs)))))

(define (get-int-reg)
  (do
    ((: free-int-regs used-regs) <- (get-state))
    (if (null? free-int-regs)
      (new-location)
      (let ((reg (car free-int-regs)))
       (do
         (set-state 'free-int-regs
                    (cdr free-int-regs))
         (unless-m (in? reg used-regs)
           (set-state 'used-regs
                      (cons reg used-regs)))
         (return reg))))))

(define (get-xmm-reg)
  (do
    ((: free-xmm-regs) <- (get-state))
    (if (null? free-xmm-regs)
      (new-location)
      (let ((reg (car free-xmm-regs)))
       (do
         (set-state 'free-xmm-regs
                    (cdr free-xmm-regs))
         (return reg))))))

(define (get-reg type)
  (case (register-type type)
    ((xmm) (get-xmm-reg))
    ((int) (get-int-reg))))

(define (get-tmp-reg)
  (do
    ((: free-int-regs used-regs tmp-regs) <- (get-state))
    (if (null? free-int-regs)
      (let ((reg (find (! (cut in? <> tmp-regs))
                       used-regs)))
        (do
          (emit `(sub 8 rsp)
                `(push ,reg))
          (set-state 'tmp-regs
                     (cons reg 'tmp-regs))
          (return reg)))
      (get-int-reg))))

(define (free-tmp-reg reg)
  (do
    ((: tmp-regs) <- (get-state))
    (if (in? reg tmp-regs)
      (do
        (set-state 'tmp-regs
                   (remove reg tmp-regs))
        (emit `(pop ,reg)
              `(add 8 rsp)))
      (free-reg reg))))

(define (free-reg x)
  (do
    ((: arch reg-args) <- (get-state))
    (cond
      ((in? x (scratch-regs arch))
       (do
         ((: free-int-regs) <- (get-state))
         (unless-m (in? x free-int-regs)
           (set-state 'free-int-regs
                      (cons x free-int-regs)))))
      ((xmm-register? x)
       (do
         ((: free-xmm-regs) <- (get-state))
         (unless-m (in? x free-xmm-regs)
           (set-state 'free-xmm-regs
                      (cons x free-xmm-regs)))))
      ((and (memory? x)
            (eqv? (operand-base x)
                  (base-reg arch))
            (>= (operand-offset x)
                (* reg-args 8))
            (eqv? (operand-modifier x) '-))
       (do
         ((: free-locations reg-args) <- (get-state))
         (unless-m (or (find (cut operand=? x <>)
                             free-locations)
                       (<= (operand-offset x)
                           (* reg-args 8)))
           (set-state 'free-locations
                      (cons x free-locations)))))
      (else (return)))))

(define (global-function? e)
  (and (eqv? (get 'expr e) 'ref)
       (eqv? (get 'type e) 'function)))

(define (compile-real-num-expr e trgt)
  (let+ (: op args expr-type) e
    (let ((op (sse-arithm-instruction
                op
                (type-size expr-type))))
      (do
        (lh <- (get-xmm-reg))
        (compile-expr (car args) lh)
        (for-each-m (lambda (x)
                      (do
                        (rh <- (compile-expr x #f))
                        (emit `(,op ,rh ,lh))
                        (free-reg rh)))
                    (cdr args))
        (if trgt
          (do
            (emit `(mov ,lh ,trgt))
            (free-reg lh)
            (return trgt))
          (if (xmm-register? lh)
            (do
              (loc <- (new-location))
              (emit `(mov ,lh ,loc))
              (free-reg lh)
              (return loc))
            (return lh)))))))

(define (const-expr? e)
  (eqv? (get 'expr e) 'const))

(define (all-const? elements)
  (all const-expr?
       elements))

(define (make-padding data)
  (let+ (sz . xs) data
    (case sz
      ((quad double)
       (list data))
      (else
        (list data
              (cons 'byte
                    (repeat 0 (- 8 (bytes sz)))))))))

(define (compile-elem-sequence e trgt)
  (define (make-c-array size elements)
    (make-data
      `((,size . ,(map (cut get 'value <>)
                       elements)))))
  (let+ (: expr type elements) e
    (let ((size (type-size type))
          (len (length elements))
          (const (all-const? elements)))
      (case expr
        ((array)
         (if const
           (do
             (dat <- (make-c-array (data-label type)
                                   elements))
             (lab <- (make-data
                       `((quad ,(bytes size)
                               ,len
                               ,len
                               ,dat))))
             (compile-symbol-ref lab trgt))
           (let ((bsize (bytes size)))
            (do
              (a <- (get-int-reg))
              (compile-mem-alloc (* bsize len) a)
              (for-each-m (lambda (e)
                            (let+ (i x) e
                              (do
                                (elt <- (compile-expr x #f))
                                (emit `(,(size-instruction 'mov size)
                                         ,(size-location elt size)
                                         (+ ,a ,(* i bsize))))
                                (free-reg elt))))
                          (enumerate elements))
              (arr <- (get-int-reg))
              (compile-mem-alloc 32 arr)
              (emit `(mov ,bsize (,arr))
                    `(mov ,len (+ ,arr 8))
                    `(mov ,len (+ ,arr 16))
                    `(mov ,a (+ ,arr 24)))
              (free-reg a)
              (return arr)))))
        ((list)
         (if const
           (do
             (lab <- (fold-m
                       (lambda (loc x)
                         (do
                           (dat <- (make-data
                                     (append
                                       (make-padding `(,(data-label type) ,(get 'value x)))
                                       `((quad ,loc)))))
                           (return dat)))
                       0
                       (reverse elements)))
             (compile-symbol-ref lab trgt))
           (let ((es (bytes size)))
            (do
              ((: arch) <- (get-state))
              (letrec
                ((compile-cons
                   (lambda (h t)
                     (do
                       (h <- (compile-expr h #f))
                       (c <- (compile-mem-alloc 16 #f))
                       (reg <- (get-tmp-reg))
                       (emit `(mov ,c ,reg)
                             `(mov ,h (,reg))
                             `(mov ,t (+ ,reg 8)))
                       (free-reg h)
                       (free-reg c)
                       (loc <- (new-location))
                       (emit `(mov ,reg ,loc))
                       (free-tmp-reg reg)
                       (return loc)))))
                (do
                  (last <- (compile-cons
                             (last-car elements)
                             0))
                  (fold-m (lambda (t h)
                            (do
                              (loc <- (compile-cons h t))
                              (free-reg t)
                              (return loc)))
                          last
                          (cdr (reverse elements)))))))))
        ((c-array)
         (if const
           (do
             (dat <- (make-c-array size elements))
             (compile-symbol-ref dat trgt))
           (throw 'not-implemented "")))))))

(define (compile-call e trgt)
  (let+ (: op args expr-type ellipsis) e
    (let* ((arg-types (parse-arg-types
                        (map (cut get 'expr-type <>)
                             args)))
           (stack-args (stack-arg-cnt arg-types))
           (reg-args (- (length arg-types)
                        stack-args))
           (xmm-cnt (count (comp (cut eqv? <> 'xmm)
                                 car)
                           arg-types))
           (ret-reg (case expr-type
                      ((real) 'xmm0)
                      (else 'rax)))
           (leaf-args (all leaf-expr? args)))
      (define (do-ellipsis)
        (when-m ellipsis
          (if (> xmm-cnt 0)
            (emit `(mov ,xmm-cnt rax))
            (emit `(xor rax rax)))))
      (do
        ((: closure arch current-name) <- (get-state))
        (op-reg
          <- (if (global-function? op)
               (return #f)
               (compile-expr op #f)))
        (if leaf-args
          (do
            (for-each-m (lambda (x)
                          (let+ ((t i) e) x
                            (compile-expr e (arg-register t i))))
                        (zip (list-head arg-types
                                        reg-args)
                             (list-head args
                                        reg-args)))
            (unless-m (= (remainder stack-args 2)
                         0)
              (emit `(sub 8 rsp)))
            (for-each-m (lambda (e)
                          (do
                            (reg <- (compile-expr e #f))
                            (emit `(pushq ,reg))
                            (free-reg reg)))
                        (reverse (list-tail args reg-args))))
          (do
            (e-args <- (map-m (cut compile-expr <> #f)
                              args))
            (for-each-m (lambda (x)
                          (let+ ((t i) e) x
                            (do
                              (emit `(mov ,e ,(arg-register t i)))
                              (free-reg e))))
                        (zip (list-head arg-types
                                        reg-args)
                             e-args))
            (unless-m (= (remainder stack-args 2)
                         0)
              (emit `(sub 8 rsp)))
            (for-each-m (lambda (x)
                          (do
                            (emit `(pushq ,x))
                            (free-reg x)))
                        (reverse (list-tail e-args reg-args)))))
        (pie <- (get-comp-opt 'opt-pie))
        (if (global-function? op)
          (let ((name (get 'value op)))
           (do
             (do-ellipsis)
             (glob <- (global-sym? name))
             (if (and pie glob)
               (emit `(call (PLT ,name)))
               (emit `(call ,name)))))
          (do
            (emit `(mov ,op-reg ,(tmp-reg arch))
                  `(lea (+ ,(tmp-reg arch) 8) ,(closure-reg arch))
                  `(mov (,(tmp-reg arch)) ,(tmp-reg arch))
                  `(call (* ,(tmp-reg arch))))  
            (free-reg op-reg)))
        (when-m (> stack-args 0)
          (emit `(add ,(+ (* stack-args
                             8)
                          (if (= (remainder stack-args 2)
                                 0)
                            0
                            8))
                      rsp)))
        (when-m closure
          (emit `(mov (CLOSURE-ADDR ,current-name)
                      ,(closure-reg arch))))
        (cond ((eqv? expr-type 'void)
               (return ret-reg))
              ((operand=? trgt ret-reg)
               (return ret-reg))
              ((not trgt)
               (do
                 (loc <- (new-location))
                 (emit `(mov ,ret-reg ,loc))
                 (return loc)))
              (else
                (do
                  (emit `(mov ,ret-reg ,trgt))
                  (return trgt))))))))

;TODO: restoring registers
(define (compile-tail-call e trgt)
  (let+ (: op args lvl) e
    (let* ((arg-types (parse-arg-types
                        (map (cut get 'expr-type <>)
                             args)))
           (stack-args (stack-arg-cnt arg-types))
           (reg-args (- (length arg-types)
                        stack-args))
           (f-name (get 'value op)))
      (do
        (e-args <- (map-m (cut compile-expr <> #f)
                          args))
        (for-each-m (lambda (e)
                      (do
                        (emit `(push ,e))
                        (free-reg e)))
                    e-args)
        (when-m (> lvl 0)
          (emit `(mov rbp r11))
          (for-each-m (lambda (_)
                        (emit `(mov (r11) r11)))
                      (range lvl))
          (emit `(mov r11 rbp)))
        (let ((len (- (length arg-types) 1)))
         (for-each-m (lambda (x)
                       (let+ (i (t idx)) x
                         (if idx
                           (emit `(pop (- rbp ,(+ 8 (* i 8)))))
                           (emit `(pop (+ rbp ,(+ 16 (* (- i reg-args) 8))))))))
                     (reverse (enumerate arg-types))))
        ((: arch closures) <- (get-state))
        (emit `(mov rbp rsp)
              `(sub (STACK-SIZE ,f-name) rsp))
        (when-m (and (> lvl 0)
                     (in? f-name closures))
          (emit `(mov (CLOSURE-ADDR ,f-name)
                      ,(closure-reg arch))))
        (emit `(jmp (TAIL ,f-name)))
        (loc <- (new-location))
        (emit `(mov rax ,loc))
        (return loc)))))

(define (compile-branch e trgt)
  (let+ (: cond true false) e
    (case (get 'expr cond)
      ((cmp)
       (compile-cmp cond trgt true false))
      ((bool)
       (compile-bool cond trgt true false))
      (else (throw (codegen-error
                     e
                     "Unknown branch type"))))))

(define (neg-cmp-expr op)
  (case op
    ((=)    '/=)
    ((/=)   '=)
    ((>)    '<=)
    ((<)    '>=)
    ((>=)   '<)
    ((<=)   '>)))

(define (neg-cmp expr)
  (set 'op
       (neg-cmp-expr (get 'op expr))
       expr))

(define (compile-cond-jmp e label)
  (let+ (: op left right arg-type) e
    (do
      (rh <- (compile-expr right #f))
      (lh <- (case arg-type
               ((real) (get-xmm-reg))
               (else (get-int-reg))))
      (compile-expr left lh)
      (case arg-type
        ((real)
         (do
           (emit `(,(sse-instruction
                      'ucomi
                      (type-size arg-type))
                    ,rh ,lh))
           (emit `(,(float-jmp-op op) ,label))))
        (else
          (do
            (emit `(cmp ,rh ,lh))
            (emit `(,(jmp-op op) ,label)))))
      (free-reg rh)
      (free-reg lh))))

(define (compile-cmp e trgt true false)
  (let+ (: op) e
    (do
      (ret <- (if trgt
                (return trgt)
                (new-location)))
      (l0 <- (make-label))
      (l1 <- (make-label))
      (compile-cond-jmp e l0)
      (compile-expr false ret)
      (emit `(jmp ,l1))
      (set-label l0)
      (compile-expr true ret)
      (set-label l1)
      (return ret))))

(define (compile-bool e trgt true false)
  (let+ (: op left right) e
    (do
      (trgt <- (if trgt
                 (return trgt)
                 (new-location)))
      (case op
        ((and)
         (do
           (l0 <- (make-label))
           (l1 <- (make-label))
           (compile-cond-jmp (neg-cmp left) l0)
           (compile-cond-jmp (neg-cmp right) l0)
           (compile-expr true trgt)
           (emit `(jmp ,l1))
           (set-label l0)
           (compile-expr false trgt)
           (set-label l1)
           (return trgt)))
        ((or)
         (do
           (l0 <- (make-label))
           (l1 <- (make-label))
           (compile-cond-jmp left l0)
           (compile-cond-jmp right l0)
           (compile-expr false trgt)
           (emit `(jmp ,l1))
           (set-label l0)
           (compile-expr true trgt)
           (set-label l1)
           (return trgt)))))))

(define (jmp-op op)
  (case op
    ((=)    'je)
    ((/=)   'jne)
    ((>)    'jg)
    ((<)    'jl)
    ((>=)   'jge)
    ((<=)   'jle)
    (else (throw 'codegen-error
                 "Unknown operator ({})"
                 op))))

(define (float-jmp-op op)
  (case op
    ((=)    'je)
    ((/=)   'jne)
    ((>)    'ja)
    ((<)    'jb)
    ((>=)   'jae)
    ((<=)   'jbe)
    (else (throw 'codegen-error
                 "Unknown operator ({})"
                 op))))

(define (make-label)
  (do
    ((: label-cnt) <- (get-state))
    (set-state 'label-cnt
               (+ label-cnt 1))
    (return (makesym ".L" label-cnt))
    ;((: code current-name) <- (get-state))
;    (let ((label
;            (makesym ".L"
;                     current-name
;                     "__"
;                     (count symbol? code))))
;     (do
;       (set-state 'code
;                  (cons label code))
;       (return label)))
    
    ))

(define (make-local-label name)
  (do
    ((: current-name) <- (get-state))
    (let ((lab (makesym "." current-name "__" name)))
     (do
       (set-label lab)
       (return lab)))))

(define (set-label lab)
  (do
    ((: code) <- (get-state))
    (set-state 'code
               (cons lab
                     (remove lab code)))))

(define (compile-mov e trgt)
  (let+ (: loc loc-type value offset ptr-type) e
    (do
      (lh <- (compile-expr value #f))
      (pie <- (get-comp-opt 'opt-pie))
      (case loc-type
        ((symbol)
         (if pie
           (do
             (reg <- (get-int-reg))
             (if-m (global-sym? loc)
               (emit `(mov (GOTPCREL ,loc) ,reg))
               (emit `(lea (+ rip ,loc) ,reg)))
             (emit `(mov ,lh (,reg)))
             (free-reg reg))
           (emit `(mov ,lh (,loc)))))
        ((ref)
         (let ((size (type-size ptr-type)))
          (do
            (reg <- (get-int-reg))
            (compile-expr loc reg)
            ((: arch) <- (get-state))
            (if (register? reg)
              (emit `(,(size-instruction 'mov size)
                       ,lh
                       (+ ,reg ,offset)))
              (emit `(mov ,reg
                          ,(tmp-reg arch))
                    `(,(size-instruction 'mov size)
                       ,lh
                       (+ ,(tmp-reg arch) ,offset))))
            (free-reg reg))))
        ((closure)
         (do
           ((: arch) <- (get-state))
           (emit `(mov ,lh
                       (+ ,(closure-reg arch)
                          ,(+ 8 (* offset 8)))))))
        (else
          (throw (codegen-error
                   e
                   "Unknown location type ({})"
                   e))))
      (return lh))))

(define (compile-cast e trgt)
  (let+ (: type value) e
    (case type
      ((real->int)
       (do
         ((: arch) <- (get-state))
         (xmm <- (get-xmm-reg))
         (compile-expr value xmm)
         (trgt <- (if trgt
                    (return trgt)
                    (new-location)))
         (if (register? trgt)
           (emit `(cvttsd2siq ,xmm ,trgt))
           (emit `(cvttsd2siq ,xmm ,(tmp-reg arch))
                 `(mov ,(tmp-reg arch) ,trgt)))
         (free-reg xmm)
         (return trgt)))
      ((int->real)
       (do
         (lh <- (compile-expr value #f))
         (trgt <- (if trgt
                    (return trgt)
                    (new-location)))
         (tmp <- (get-xmm-reg))
         (emit `(cvtsi2sdq ,lh ,tmp)
               `(mov ,tmp ,trgt))
         (free-reg tmp)
         (free-reg lh)
         (return trgt))))))

(define (compile-deref e trgt)
  (let+ (: loc ptr-type offset) e
    (let ((size (type-size ptr-type)))
     (do
       ((: arch) <- (get-state))
       (compile-expr loc (tmp-reg arch))
       (trgt <- (get-reg ptr-type))
       (when-m (and (not (eqv? size 'quad))
                    (register? trgt))
         (emit `(xor ,trgt ,trgt)))
       (emit `(,(case (register-type ptr-type)
                  ((int)
                   (size-instruction 'mov size))
                  ((xmm)
                   (sse-instruction 'mov size)))
                ,(if offset
                   `(+ ,(tmp-reg arch) ,offset)
                   `(,(tmp-reg arch)))
                ,trgt))
       (return trgt)))))

(define (compile-mem-alloc sz trgt)
  (compile-call
    `((expr . call)
      (op . ((expr . ref)
             (type . function)
             (value . mem_alloc)))
      (args . (((expr . const)
                (type . int)
                (value . ,sz))))
      (expr-type . (: ptr void)))
    trgt))

(define (compile-struct e trgt)
  (let+ (: elements) e
    (if (all-const? elements)
      (do
        (lab <- (make-data
                  (mappend (lambda (x)
                             (let+ (: expr-type value) x
                               (make-padding
                                 (list (data-label expr-type)
                                       value))))
                           elements)))
        (compile-symbol-ref lab trgt))
      (let ((struct-type (map (cut get 'expr-type <>) elements)))
       (do
         (e-args <- (map-m (cut compile-expr <> #f)
                           elements))
         (tmp <- (get-tmp-reg))
         (compile-mem-alloc (* 8 (length elements))
                            tmp)
         (fold-m (lambda (o x)
                   (let+ (arg sz) x
                     (do
                       (emit `(,(size-instruction 'mov sz)
                                ,arg
                                (+ ,tmp ,o)))
                       (free-reg arg)
                       ;(return (+ o (bytes sz)))
                       (return (+ o 8))
                       )))
                 0
                 (zip e-args
                      (map (comp type-size
                                 (cut get 'expr-type <>))
                           elements)))
         (loc <- (new-location))
         (emit `(mov ,tmp ,loc))
         (free-tmp-reg tmp)
         (return loc))))))

(define (compile-closure e trgt)
  (let+ (: fn args) e
    (do
      (f <- (compile-symbol-ref fn #f))
      (e-args <- (map-m (cut compile-expr <> #f)
                        args))
      (tmp <- (get-tmp-reg))
      ((: arch) <- (get-state))
      (compile-mem-alloc (+ 16 (* 8 (length args)))
                         tmp)
      (emit `(mov ,f (,tmp))
            `(mov ,(closure-reg arch)
                  (+ ,tmp 8)))
      (free-reg f)
      (fold-m (lambda (o x)
                (let+ (arg sz) x
                  (do
                    (emit `(,(size-instruction 'mov sz)
                             ,arg
                             (+ ,tmp ,o)))
                    (free-reg arg)
                    (return (+ o 8)))))
              16
              (zip e-args
                   (map (comp type-size
                              (cut get 'expr-type <>))
                        args)))
      (loc <- (new-location))
      (emit `(mov ,tmp ,loc))
      (free-tmp-reg tmp)
      (return loc))))

(define (compile-nop e trgt)
  (do
    (return trgt)))

(define (compile-neg e trgt)
  (let+ (: sub) e
    (do
      (reg <- (compile-expr sub trgt))
      (emit `(xor 1 ,reg))
      (return reg))))

(define (compile-null e trgt)
  (return 0))

(define compile-expr-dispatch
  `((const
      (amd64 . ,compile-const)
      (x86 . ,compile-const))
    (sequence
      (amd64 . ,compile-sequence)
      (x86 . ,compile-sequence))
    (ref
      (amd64 . ,compile-ref)
      (x86 . ,compile-ref))
    (int-num-expr
      (amd64 . ,compile-int-num-expr)
      (x86 . ,compile-int-num-expr))
    (real-num-expr
      (amd64 . ,compile-real-num-expr)
      (x86 . ,compile-real-num-expr))
    (array
      (amd64 . ,compile-elem-sequence)
      (x86 . ,compile-elem-sequence))
    (c-array
      (amd64 . ,compile-elem-sequence)
      (x86 . ,compile-elem-sequence))
    (list
      (amd64 . ,compile-elem-sequence)
      (x86 . ,compile-elem-sequence))
    (call
      (amd64 . ,compile-call)
      (x86 . ,compile-call))
    (tail-call
      (amd64 . ,compile-tail-call)
      (x86 . ,compile-tail-call))
    (branch
      (amd64 . ,compile-branch)
      (x86 . ,compile-branch))
    (set
      (amd64 . ,compile-mov)
      (x86 . ,compile-mov))
    (cast
      (amd64 . ,compile-cast)
      (x86 . ,compile-cast))
    (deref
      (amd64 . ,compile-deref)
      (x86 . ,compile-deref))
    (struct
      (amd64 . ,compile-struct)
      (x86 . ,compile-struct))
    (closure
      (amd64 . ,compile-closure)
      (x86 . ,compile-closure))
    (nop
      (amd64 . ,compile-nop)
      (x86 . ,compile-nop))
    (neg
      (amd64 . ,compile-neg)
      (x86 . ,compile-neg))
    (null
      (amd64 . ,compile-null)
      (x86 . ,compile-null))))

(define (compile-expr e trgt)
  (let+ (: expr file line col expr-type) e
    (do
      (dbg <- (get-comp-opt 'opt-dbg))
      ((: arch) <- (get-state))
      (when-m (and dbg file line col)
        ((: files) <- (get-state))
        (set-label (fmt #f "\t.loc\t{} {} {}"
                        (+ 1 (index (cut string=? file <>)
                                        files))
                        line
                        col)))
      (let ((func (get `(,expr ,arch)
                       compile-expr-dispatch)))
       (if func
         (do
           (reg <- (func e trgt))
           (if (or (not trgt)
                   (operand=? reg trgt))
             (return reg)
             (do
               (unless-m (eqv? expr-type 'void)
                 (emit `(mov ,reg ,trgt)))
               (return trgt))))
         (throw (codegen-error e "Unknown expression type ({})" expr)))))))

(define (call-with-fresh-state proc)
  (do
    ((: (old-code code)) <- (get-state))
    (set-state 'code '())
    proc
    ((: code) <- (get-state))
    (set-state 'code old-code)
    (return (reverse code))))

(define (make-prologue)
  (call-with-fresh-state
    (do
      ((: arg-types stack-depth reg-args current-name
        used-regs tco-labels arch closure leaf)
       <- (get-state))
      (case arch
        ((amd64)
         (emit `(push rbp)
               "\t.cfi_def_cfa_offset\t16"
               "\t.cfi_offset\t%rbp, -16"
               `(mov rsp rbp)
               "\t.cfi_def_cfa_register\t%rbp"))
        ((x86)
         (emit `(push ebp)
               "\t.cfi_def_cfa_offset\t8"
               `(mov esp ebp)
               "\t.cfi_def_cfa_register\t%ebp")))
      (let* ((full-stack-depth
               (+ stack-depth (* (length used-regs) 8)))
             (full-stack-depth
               (+ full-stack-depth
                  (abs (remainder full-stack-depth 16)))))
        (do
          (when-m (> stack-depth 0)
            (emit `(sub ,full-stack-depth
                        rsp)))
          (set-state `(stack-depths ,current-name)
                     full-stack-depth)
          (for-each-m (lambda (x)
                        (let ((reg (cadr x))
                              (loc `(- rbp ,(+ stack-depth
                                               8
                                               (* 8 (car x))))))
                          (do
                            (emit `(mov ,reg ,loc))
                            (when-m (eqv? reg (closure-reg arch))
                              (set-state `(closure-reg ,current-name)
                                         (caddr loc))))))
                      (enumerate used-regs))
          (unless-m leaf
            (for-each-m (lambda (x)
                          (let+ (idx (type i)) x
                            (emit `(mov ,(arg-register type i)
                                        (- rbp ,(+ 8 (* idx 8)))))))
                        (filter (comp cadr cadr)
                                (enumerate arg-types))))
          (set-label (get current-name tco-labels))
          (dbg <- (get-comp-opt 'opt-dbg))
          (when-m dbg
            (let ((tdbg (makesym "." current-name "__TCO_DEBUG")))
             (do
               (set-label tdbg)
               (push-state 'globals tdbg)))))))))

(define (make-epilogue)
  (call-with-fresh-state
    (do
      ((: used-regs stack-depth) <- (get-state))
      (for-each-m (lambda (x)
                    (emit `(mov (- rbp ,(+ stack-depth
                                           8
                                           (* 8 (car x))))
                                ,(cadr x))))
                  (enumerate used-regs))
      (emit `(leave)
            "\t.cfi_def_cfa %rsp, 8"
            `(ret)))))

(define (finalize)
  (do
    ((: code) <- (get-state))
    (pro <- (make-prologue))
    (epi <- (make-epilogue))
    (peep <- (get-comp-opt 'opt-code-opt))
    (return (append pro
                    (if peep
                      (peephole-optimize
                        (reverse code))
                      (reverse code))
                    epi))))

(define (reset-state name arg-types ret)
  (let* ((ret-reg (case (register-type ret)
                    ((xmm) 'xmm0)
                    ((int) 'rax)))
         (arg-cnt (length arg-types))
         (stack-args (stack-arg-cnt arg-types))
         (reg-args (- arg-cnt
                      stack-args)))
    (do
      ((: code arch closure leaf) <- (get-state))
      (unless-m (null? code)
        (throw 'error "Previous function was not finalized"))
      (set-state 'current-name name)
      (set-state 'arg-types arg-types)
      (set-state 'ret-reg ret-reg)
      (set-state 'stack-depth (if leaf 0 (* 8 reg-args)))
      (set-state 'stack-args stack-args)
      (set-state 'reg-args reg-args)
      (set-state 'free-int-regs
                 (scratch-regs arch))
      (set-state 'free-xmm-regs
                 (map xmm-register (range 16)))
      (set-state 'tmp-regs
                 '())
      (set-state 'free-locations
                 '())
      (set-state 'arg-locations
                 (map (lambda (x)
                        (let+ (x (type idx)) x
                          (if idx
                            (if leaf
                              (arg-register type idx)
                              `(- rbp
                                  ,(+ 8 (* x 8))))
                            `(+ rbp
                                ,(+ 16 (* (- x reg-args)
                                          8))))))
                      (enumerate arg-types)))
      (set-state 'used-regs (if closure
                              `(,(closure-reg arch))
                              '())))))

(define (add-chunk name code)
  (do
    (push-state `(functions)
                (cons name code))
    (set-state 'code '())))

(define (compile-function f)
  (let+ (name type closure . expr) f
    (let+ (args ret) (split-on arrow? type)
      (do
        (set-state 'closure closure)
        (opt-leaf <- (get-comp-opt 'opt-leaf))
        (set-state 'leaf (and opt-leaf (leaf-expr? expr)))
        (reset-state name
                     (parse-arg-types args)
                     (car ret))
        ((: ret-reg toplevel opts) <- (get-state))
        (tco-lab <- (make-label))
        (set-state `(tco-labels ,name) tco-lab)
        (top-init
          <- (if (eqv? name toplevel)
               (do
                 (top-init <- (make-data `((quad 0))))
                 (l0 <- (make-label))
                 (compile-symbol-value-ref top-init 'rax)
                 (emit `(cmp 0 rax)
                       `(je ,l0)
                       `(mov rbp rsp)
                       `(pop rbp)
                       `(ret))
                 (set-label l0)
                 (return top-init))
               (return #f)))
        (compile-expr expr
                      (size-location ret-reg
                                     (type-size (get 'expr-type expr))))
        (when-m top-init
          (compile-expr `((expr . set)
                          (loc-type . symbol)
                          (loc . ,top-init)
                          (value . ((expr . const)
                                    (type . int)
                                    (value . 1))))
                        #f))
        (code <- (finalize))
        (add-chunk name code)))))

(define (segment-size x)
  (foldr +
         0
         (map (lambda (x)
                (* (bytes (car x))
                   (length (cdr x))))
              x)))

(define (register-type x)
  (case x
    ((real float double) 'xmm)
    (else 'int)))

(define (arg-register-cnt x)
  (case x
    ((xmm) 16)
    ((int) 6)))

(define (parse-arg-types type)
  (reverse
    (foldl (lambda (xs x)
             (let ((last (find (comp (cut eqv? <> x)
                                     car)
                               xs)))
               (cons (list x
                           (if last
                             (if (and (number? (cadr last))
                                      (< (+ 1 (cadr last))
                                         (arg-register-cnt x)))
                               (+ 1 (cadr last))
                               #f)
                             0))
                     xs)))
           '()
           (map register-type type))))

(define (stack-arg-cnt type)
  (count (comp not cadr) type))

(define (arg-register type i)
  (case type
    ((int)
     (list-ref (int-arg-registers) i))
    ((xmm)
     (and (< i 16)
          (makesym "xmm" i)))))

(define (data-label t)
  (case t
    ((real double) 'double)
    ((float) 'float)
    (else (type-size t))))

(define (byte-size t)
  (bytes (type-size t)))

(define (compile-unit u)
  (let+ (: functions externs data globals tco filename files opts toplevel libs) u
    (eval-state (do
                  (for-each-m (lambda (x)
                                (let+ (name _ closure) x
                                  (when-m closure
                                    (push-state 'closures name))))
                              functions)
                  (for-each-m compile-function
                              (filter (comp (! (cut eqv? <> toplevel))
                                            car)
                                      functions))
                  (for-each-m (lambda (x)
                                (let+ (name . (: type value)) x
                                  (set-state `(data ,name)
                                             `((,(type-size type) . ,(mklist value))))))
                              data)
                  (let ((top (find (comp (cut eqv? <> toplevel)
                                         car)
                                   functions)))
                    (if top
                      (compile-function top)
                      (return
                        (fmt #t "Warning: toplevel function is undefined.\n"))))
                  ((: functions data) <- (get-state))
                  (set-state 'functions
                             (append
                               (filter (! (comp (cut eqv? <> toplevel)
                                                car))
                                       functions)
                               (list
                                 (find (comp (cut eqv? <> toplevel)
                                             car)
                                       functions))))
                  (set-state 'data (reverse data)))
                `((extern . ,(if (get 'opt-gc opts)
                               (cons (string->symbol "GC_add_roots")
                                     externs)
                               externs))
                  (globals . ,globals)
                  (label-cnt . 0)
                  (tco-labels)
                  (closure-reg)
                  (closures)
                  (files . ,files)
                  (stack-depth . 0)
                  (stack-depths)
                  (libs . ,libs)
                  (filename . ,filename)
                  (arch . ,(let ((arch (makesym (get 'arch opts))))
                             (if (in? arch '(amd64 x86))
                               arch
                               (throw 'error "Unknown arch ({})" arch))))
                  (functions)
                  (data)
                  (opts . ,opts)
                  (toplevel . ,toplevel)
                  (code)))))

(define (ins->string ins u)
  (cond ((symbol? ins)
         (concat ins ":"))
        ((pair? ins)
         (let+ (op . args) ins
           (try
             (lambda ()
               (fmt #f "\t{}\t{}"
                    op
                    (join-str ", "
                              (map (cut op->string <> u)
                                   args))))
             (lambda (err)
               (throw 'codegen-error
                      "Invalid instruction ({})"
                      ins)))))
        (else (throw 'codegen-error
                     "Invalid instruction ({})"
                     ins))))

(define (op->string x u)
  (cond ((symbol? x)
         (if (or (register? x)
                 (xmm-register? x))
           (concat "%" x)
           (symbol->string x)))
        ((integer? x)
         (concat (if (< x 0)
                   "$-0x"
                   "$0x")
                 (number->string (abs x) 16)))
        ((pair? x)
         (if (and (symbol? (car x))
                  (null? (cdr x)))
           (fmt #f "({})" (op->string (car x) u))
           (case (car x)
             ((TAIL)
              (fmt #f "{}" (get `(tco-labels ,(cadr x)) u)))
             ((STACK-SIZE)
              (op->string (get `(stack-depths ,(cadr x)) u)
                          u))
             ((CLOSURE-ADDR)
              (op->string `(- rbp ,(get `(closure-reg ,(cadr x)) u))
                          u))
             ((GOTPCREL)
              (fmt #f "{}@GOTPCREL(%rip)" (cadr x)))
             ((PLT)
              (fmt #f "{}@PLT" (cadr x)))
             ((+ -)
              (fmt #f "{}{}({})"
                   (case (car x)
                     ((+) "")
                     ((-) "-"))
                   (caddr x)
                   (op->string (cadr x) u)))
             ((* $)
              (concat (car x)
                      (op->string (cadr x) u)))
             (else (throw 'error "")))))
        (else (throw 'error ""))))

(define (dump-code u out)
  (let+ (: files filename functions globals data extern opts libs) u
    (define (line s . args)
      (apply fmt out (concat s "\n") args))
    (define (tline s . args)
      (apply fmt out (concat "\t" s "\n") args))
    (define (dump-data x)
      (let+ (name . rest) x
        (line "")
        (when (in? name globals)
          (tline ".globl {}" name))
        (tline ".type {},@object" name)
        (let ((size (segment-size rest)))
         (tline ".align {}" 16)
         (when (>= size 4)
           (tline ".align {}"
                  (cond ((>= size 16) 16)
                        ((>= size 8) 8)
                        (else 4))))
         (tline ".size {},{}"
                name
                (segment-size rest)))
        (line "{}:" name)
        (for-each (lambda (x)
                    (let+ (type . value) x
                      (tline ".{} {}"
                            type
                            (join-str ","
                                      (map repr value)))))
                  rest)))
    (define (dump-function f)
      (let+ (name . code) f
            (line "")
            (when (in? name globals)
              (tline ".globl {}" name))
            (tline ".align 16,0x90")
            (tline ".type {},@function" name)
            (line "{}:" name)
            (tline ".cfi_startproc")
            (for-each (disj (conj string?
                                  (cut line "{}" <>))
                            (comp (cut line "{}" <>)
                                  (cut ins->string <> u)))
                      code)
            (tline ".cfi_endproc")  
            (tline ".size {}, .-{}" name name)))
    (tline ".file \"{}\""
          filename)
    (when (get 'opt-dbg opts)
      (for-each (cut apply
                     (cut tline ".file {} \"{}\""
                          <>
                          <>)
                     <>)
                (enumerate files 1)))
    (tline ".data")
    (for-each dump-data data)
    (tline ".text")
    (for-each dump-function functions)
    (tline ".ident\t\"{}: {}\""
           package-name
           package-version)
    (tline ".section\t.note.GNU-stack,\"\",@progbits")))

(define (write-code u file-name)
  (call-with-output-file
    file-name
    (cut dump-code u <>)))

(define (instruction-name x)
  (and (pair? x)
       (let ((n (symbol->string (car x))))
        (if (in? (string-ref n (- (string-length n)
                                  1))
                 '(#\q #\l #\w #\b))
          (let ((n (makesym
                     (substr n 0 (- (string-length n) 1)))))
            (if (in? n '(mov add sub mul div imul idiv lea xor ucomi cmp push pop))
              n
              (car x)))
          (car x)))))

(define instruction=?
  (case-lambda
    ((a b)
     (let ((a (instruction-name a))
           (b (instruction-name b)))
       (and a b (eqv? a b))))
    ((a b i)
     (and (pair? a)
          (pair? b)
          (eqv? (car a)
                (car b))
          (let ((a (instruction-name a))
                (b (instruction-name b)))
            (and (eqv? a i)
                 (eqv? b i)))))))

(define (peephole-optimize code)
  (define (optimize code)
    (let loop ((tail code)
               (res '()))
      (cond ((null? tail)
             (reverse res))
            ((null? (cdr tail))
             (reverse
               (cons (car tail) res)))
            (else
              (let+ (a b . rest) tail
                (cond ((and (eqv? (instruction-name a) 'mov)
                            (pair? b)
                            (in? (instruction-name b)
                                 '(mov add sub imul or and xor))
                            (eqv? (caddr a)
                                  (cadr b))
                            (or (register? (cadr a))
                                (register? (caddr b))))
                       (loop rest
                             (cons `(,(car b) ,(cadr a) ,(caddr b))
                                   res)))
                      ((and (eqv? (instruction-name a) 'mov)
                            (or (operand=? (cadr a) (caddr a))
                                (register=? (cadr a) (caddr a))))
                       (loop (cdr tail) res))
                      (else (loop (cdr tail)
                                  (cons a res)))))))))
  (let ((new (optimize code)))
   (if (= (length new) (length code))
     new
     (peephole-optimize new))))
