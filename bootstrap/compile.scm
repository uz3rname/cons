;-unit compile -uses typecheck -uses codegen
(include "inc/macro.scm")
;(import foreign)
(import (chicken process-context posix)
        (chicken process)
        (chicken file))

(foreign-declare
  "#include \"inc/decls.h\"")
(define dump-section
  (foreign-lambda c-string "dump_section" c-string c-string))
(define dump-symbol
  (foreign-lambda c-string "dump_symbol" c-string c-string))

(define (expr->bool expr)
  (let+ (: tag value sub) expr
    (case tag
      ((cmp-expr)
       `((expr . cmp)
         (op . ,value)
         (left . ,(expr->ir (car sub)))
         (right . ,(expr->ir (cadr sub)))))
      (else
        `((expr . cmp)
          (op . /=)
          (left .  ,(expr->ir expr))
          (right . ((expr . const)
                    (value . 0)
                    (type . int))))))))

(define (expr->ir expr)
  (unless (get 'type expr)
    (throw (expr-error 'error
                       expr
                       "Expression is untyped")))
  (let+ (: tag value sub) expr
    (let ((e (case tag
               ((sequence)
                `((expr . sequence)
                  (sub . ,(filter (! null?)
                                  (map expr->ir sub)))))
               ((int)
                `((expr . const)
                  (value . ,value)
                  (type . int)))
               ((bool)
                `((expr . const)
                  (value . ,(if value 1 0))
                  (type . int)))
               ((string)
                `((expr . array)
                  (type . char)
                  (elements . ,(map (lambda (x)
                                      `((expr . const)
                                        (type . int)
                                        (value . ,(char->integer x))))
                                    (string->list value)))))
               ((char)
                `((expr . const)
                  (value . ,(char->integer value))
                  (type . int)))
               ((real)
                `((expr . const)
                  (type . real)
                  (value . ,value)))
               ((void)
                '((expr . nop)))
               ((symbol)
                (let+ (: ref value lvl arg-types) (get 'ref expr)
                  `((expr . ref)
                    (type . ,ref)
                    (lvl . ,lvl)
                    (arg-types . ,arg-types)
                    (value . ,(fcase value
                                     ((symbol?) (c-ify-name value))
                                     (else value))))))
               ((function)
                `((expr . ref)
                  (type . ,(if (get 'ptr expr)
                             'function-ptr
                             'function))
                  (value . ,(fcase value
                                   ((symbol?) (c-ify-name value))
                                   (else value)))))
               ((if)
                (let+ (cnd true false) sub
                  `((expr . branch)
                    (cond . ,(expr->bool cnd))
                    (true . ,(expr->ir true))
                    (false . ,(expr->ir false)))))
               ((cmp-expr)
                `((expr . branch)
                  (cond . ((expr . cmp)
                           (op . ,value)
                           (arg-type . ,(get 'arg-type expr))
                           (left . ,(expr->ir (car sub)))
                           (right . ,(expr->ir (cadr sub)))))
                  (true . ((expr . const) (value . 1) (type . int)))
                  (false . ((expr . const) (value . 0) (type . int)))))
               ((bool-expr)
                (case value
                  ((not)
                   `((expr . neg)
                     (sub . ,(expr->ir (car sub)))))
                  (else
                    `((expr . branch)
                      (cond . ((expr . bool)
                               (op . ,value)
                               (left . ,(expr->bool (car sub)))
                               (right . ,(expr->bool (cadr sub)))))
                      (true . ((expr . const) (value . 1) (type . int)))
                      (false . ((expr . const) (value . 0) (type . int)))))))
               ((call)
                `((expr . ,(if value 'tail-call 'call))
                  (op . ,(expr->ir (car sub)))
                  (ellipsis . ,(get 'ellipsis expr))
                  (args . ,(map expr->ir (cdr sub)))
                  (lvl . ,value)))
               ((num-expr)
                `((expr . ,(case (expr-type expr)
                             ((int ptr) 'int-num-expr)
                             ((real) 'real-num-expr)))
                  (op . ,value)
                  (args . ,(map expr->ir sub))))
               ((array c-array list)
                `((expr . ,tag)
                  (type . ,(get 'element-type expr))
                  (elements . ,(map expr->ir sub))))
               ((null)
                `((expr . null)))
               ((closure)
                `((expr . closure)
                  (fn . ,(c-ify-name value))
                  (args . ,(map expr->ir sub))))
               ((cast)
                (let* ((e (car sub))
                       (from (expr-type e)))
                  (cond 
                    ((and (eqv? from 'int)
                          (eqv? value 'real))
                     `((expr . cast)
                       (type . int->real)
                       (value . ,(expr->ir e))))
                    ((and (eqv? from 'real)
                          (eqv? value 'int))
                     `((expr . cast)
                       (type . real->int)
                       (value . ,(expr->ir e))))
                    (else (expr->ir e)))))
               ((struct)
                `((expr . struct)
                  (elements . ,(map expr->ir sub))))
               ((ref)
                (let+ (: type struct-type) expr
                  `((expr . deref)
                    (loc . ,(expr->ir (car sub)))
                    (ptr-type . ,type)
                    ;(offset . ,(struct-offset struct-type value))
                    (offset . ,(* value 8))
                    ))
                ;                `((expr . struct-ref)
                ;                  (struct . ,(expr->ir (car sub)))
                ;                  (idx . ,value))
                )
               ((set)
                (let+ (: loc-type) expr
                  (append
                    `((expr . set)
                      (loc-type . ,loc-type)
                      (value . ,(expr->ir (car sub))))
                    (case loc-type
                      ((ref)
                       `((loc . ,(expr->ir (cadr sub)))
                         (ptr-type . ,(get 'ptr-type expr))
;                         (offset . ,(struct-offset
;                                      (get 'struct-type expr)
;                                      value))
                         (offset . ,(* value 8))))
                      ((closure)
                       `((offset . ,value)))
                      ((symbol)
                       `((loc . ,(c-ify-name value))))))))
               ((deref)
                (let ((pt (get 'ptr-type expr)))
                 `((expr . deref)
                   (loc . ,(expr->ir (car sub)))
                   (ptr-type . ,pt)
                   (offset . ,value))))
               ((offset)
                `((expr . int-num-expr)
                  (op . +)
                  (args . ,(list (expr->ir (car sub))
                                 (expr->ir
                                   (if value
                                     (make-expr
                                       (expr-file expr)
                                       (expr-line expr)
                                       (expr-col expr)
                                       'int
                                       value
                                       '()
                                       'type 'int)
                                     (cadr sub)))))))
               (else (throw 'not-implemented
                            "IR transformation for expression of type {} {}"
                            tag
                            (expr-repr expr))))))
      (-> e
        (set 'file (expr-file expr) <>)
        (set 'line (expr-line expr) <>)
        (set 'col (expr-col expr) <>)
        (set 'expr-type (expr-type expr) <>)))))

(define (global-list unit)
  (let+ (: export functions generics hidden export-all data) unit
    (let* ((scope (get `(,(get 'toplevel unit) scope)
                       functions)))
      (filter id (map (lambda (name)
                        (and-let* ((x (get name scope)))
                          (let+ (: ref value) x
                            (case ref
                              ((generic)
                               (cons
                                 name
                                 (map
                                   c-ify-name
                                   (filter (! (comp (cut get 'extern <>)
                                                  (cut get <> functions)))
                                           (map cadr
                                                (get `(,value functions)
                                                     generics))))))
                              ((data)
                               (and (not (get 'extern (get value data)))
                                    (list name (c-ify-name value))))))))
                      (if export-all
                        (filter (! (cut in? <> hidden))
                                (map car scope))
                        export))))))

(define (unit->ir unit)
  (let+ (: functions generics options
         data toplevel tco filename type-env libs) unit
    `((externs
        . ,(map car
                (append (filter extern?
                                functions)
                        (filter extern?
                                data))))
      (globals . ,(cons toplevel
                        (apply append
                               (map cdr (global-list unit)))))
      (toplevel . ,toplevel)
      (tco . ,(map c-ify-name tco))
      (opts . ,options)
      (files . ,(-> (tree-search expr? unit)
                  (map expr-file <>)
                  (filter string? <>)
                  (unique <> string=?)))
      (filename . ,filename)
      (libs . ,libs)
      (functions
        . ,(reverse
             (map (lambda (x)
                    (let+ (name . (: type body closure-env)) x
                      (cons* (c-ify-name name)
                             (lookup-type type
                                          type-env)
                             (not (null? closure-env))
                             (try
                               (lambda ()
                                 (expr->ir body))
                               (lambda (err)
                                 (throw
                                   'error
                                   "In function {}: Error({}): {}"
                                   name
                                   (error-type err)
                                   (error-msg err)))))))
                  (filter (! extern?)
                          functions))))
      (data . ,(map (lambda (x)
                      (let+ (name . data) x
                        (cons (c-ify-name name)
                              data)))
                    (filter (! extern?) data))))))

(define (dump-data x f)
  (let+ (name . (: type)) x
    (fmt f "(extern {} ((:: {})))\n"
            name
            (type-repr type))))

(define (dump-generic x f all-functions)
  (let+ (name . (: functions templates)) x
    (let ((templates (filter (! (cut get 'imported <>))
                             templates))
          (functions (filter (comp (! (cut get <> all-functions))
                                 (cut cons <> '(imported))
                                 cadr)
                             functions)))
      (unless (null? functions)
        (fmt f "(extern {}" name)
        (for-each (lambda (func)
                    (fmt f "\n  ((:: {}) \"{}\")"
                         (join-str " " (map type-repr (car func)))
                         (c-ify-name (cadr func))))
                  functions)
        (fmt f ")\n"))
      (for-each (lambda (tpl)
                  (let+ (: arg-list type body) tpl
                    (fmt f "(def {} {} (:: {})\n;{}\n  {})\n"
                         name
                         arg-list
                         (join-str " " (map type-repr type))
                         (fmt #f "#file: \"{}\" line: {}"
                              (expr-file body)
                              (expr-line body))
                         (join-str " " (map expr-repr body)))))
                templates))))

(define (dump-typedef x f)
  (let+ (name . (: type alias fields args union)) x
    (fmt f "({} {} {})\n"
            (if alias "alias" "type")
            (if args
              (format-str "({} {})"
                          name
                          (join-str " " (map repr args)))
              name)
            (cond
              (fields
                (concat
                  "#("
                  (join-str
                    " " (map (cut format-str "(.{} {})" <> <>)
                             fields
                             (map type-repr type)))
                  ")"))
              (union
                (concat
                  "(or "
                  (join-str
                    " " (map (cut format-str "(.{} {})" <> <>)
                             union
                             (map type-repr (cdr type))))
                  ")"))
              (else (type-repr (or alias type)))))))

(define (dump-macrodef x f)
  (let+ (name . (: patterns imported)) x
    (when (and patterns (not imported))
      (fmt f "(macro {}" name)
      (for-each (lambda (x)
                  (fmt f "\n  ({}\n   {})"
                          (car x)
                          (expr-repr (cdr x))))
                patterns)
      (fmt f ")\n"))))

(define (dump-header u out)
  (let ((globals (map car (global-list u))))
   (let+ (: generics toplevel data type-env functions
          macro-env export hidden export-all mod-name libs using) u
     (let ((f (open-output-file out)))
      (when (not mod-name)
        (throw 'error "Module name is not defined"))
      (for-each (cut fmt f "(use {})\n") using)
      (fmt f "(declare (link-with \"{}.a\" {}))\n"
           mod-name
           (join-str " "
                     (map (cut fmt #f "\"{}\"" <>)
                          (unique libs string=?))))
      (fmt f "(declare (import-begin))\n")
      (for-each (cut dump-macrodef <> f)
                (reverse
                  (filter (disj (conj (const export-all)
                                      (comp (! (cut in? <> hidden))
                                          car))
                                (comp (cut in? <> export)
                                    car))
                          macro-env)))
      (for-each (cut dump-typedef <> f)
                (reverse
                  (filter
                    (conj (disj (conj (const export-all)
                                      (comp (! (cut in? <> hidden))
                                          car))
                                (comp (cut in? <> export)
                                    car))
                          (comp (! (cut in? <> (builtin-types)))
                              car)
                          (comp (! (cut get 'imported <>))
                              cdr))
                    (get 'type-table type-env))))
      (for-each (cut dump-data <> f)
                (reverse (filter (comp (cut in? <> globals)
                                     car)
                                 data)))
      (for-each (cut dump-generic <> f functions)
                (reverse (filter (comp (conj (cut in? <> globals)
                                           (! (cut eqv? <> toplevel)))
                                     car)
                                 generics)))
      (fmt f "(extern {} ((:: -> void)))\n"
              toplevel)
      (fmt f "(declare (import-end))\n")
      (fmt f "({})\n" toplevel)
      (close-output-port f)))))

(define (file->unit filename options)
  (make-expr
    filename 0 0
    'unit filename
    (parse-file filename (get 'inc options))))

(define (string->unit s options)
  (make-expr
    "<stdin>" 0 0
    'unit "<stdin>"
    (parse-string s "<stdin>" (get 'inc options))))

(define (load-source filename options)
  (-> (scase filename
        (("-")
         (letrec ((loop (lambda (result)
                          (let ((ch (read-char)))
                           (if (eof-object? ch)
                             (list->string (reverse result))
                             (loop (cons ch result)))))))
           (-> (loop '())
             (string->unit <> options))))
        (else (file->unit filename options)))
    (eval-unit <>
               (-> options
                 (set 'toplevel
                      (if (get 'mod options)
                        (string->symbol
                          (concat "toplevel_"
                                  (get 'mod-name options)))
                        (let ((name (get 'toplevel-name options)))
                         (if name
                           (string->symbol name)
                           'toplevel)))
                      <>)
                 (set 'inc
                      (cons (file-directory filename)
                            (get 'inc options))
                      <>)))
    type-check-unit))

(define (compile-source filename options)
  (-> (load-source filename options)
    (unit->ir <>)
    (compile-unit <>)))

(define (shell-exec options str . rest)
  (let ((cmd (apply format-str str rest)))
   (when (get 'verbose options)
     (fmt #t "$ {}\n" cmd))
   (system cmd)))

(define (write-asm u f options)
  (let+ (: mod opt-dbg obj) options
    (-> (unit->ir u)
      (compile-unit <>)
      (dump-code <> f opt-dbg))))

(define (tmp-file-name orig suffix)
  (fmt #f "/tmp/{}-{}{}"
       (drop-file-suffix
         (file-base-name orig))
       (current-process-id)
       suffix))

(define (dump-obj u out options)
  (let ((asm (tmp-file-name out ".s")))
   (write-code (compile-unit (unit->ir u))
               asm)
   (try
     (lambda ()
       (make-obj-from-asm asm out options))
     (lambda (e) (throw e))
     (lambda ()
       (safe-delete asm)))))

(define (make-asm-from-src in out options)
  (let* ((out (or out (set-file-suffix in ".s")))
         (u (compile-source in options)))
    (scase out
      (("-")
       (dump-code u #t))
      (else (write-code u out)))
    out))

(define (make-obj-from-asm in out options)
  (let ((out (or out (set-file-suffix in ".o"))))
   (shell-exec
     options
     "{} {} -o {}"
     as-program
     in
     out)
   (if (file-exists? out)
     out
     (throw 'error "Linker error"))))

(define (make-obj-from-src in out options)
  (let ((asm (make-asm-from-src
               in
               (tmp-file-name in ".s")
               options)))
    (try
      (lambda ()
        (make-obj-from-asm
          asm
          (or out (set-file-suffix in ".o"))
          options))
      (lambda (e) (throw e))
      (lambda ()
        (safe-delete asm)))))

(define (safe-delete f)
  (when (file-exists? f)
    (if (directory-exists? f)
      (delete-directory f)
      (delete-file f))))

(define (dump-module in out options)
  (let* ((dir (tmp-file-name "" ""))
         (u (load-source
              in
              options))
         (obj (fmt #f "{}/{}.o"
                   dir
                   (drop-file-suffix
                     (file-base-name in))))
         (header (fmt #f "{}/.header" dir)))
    (try
      (lambda ()
        (create-directory dir)
        (dump-obj u obj options)
        (dump-header u header)  
        (shell-exec options
                    "{} rcs {} {} {}"
                    ar-program
                    out
                    header
                    obj))
      (lambda (e) (throw e))
      (lambda ()
        (safe-delete obj)
        (safe-delete header)
        (safe-delete dir)))))

(define (build-executable obj-files libs out options)
  (let+ (: lib-dir exec ld-opts opt-static opt-pie opt-ld opt-ld-so) options
    (shell-exec
      options
      "{} -dynamic-linker {} {} {} {} {} {} --as-needed {} -o {}"
      (or opt-ld ld-program)
      (or opt-ld-so
          ld-so-path)
      (join-str " " ld-opts)
      (join-str " "
                (map (cut concat "-L" <>)
                     lib-dir))
      (if opt-static
        "-static "
        "")
      (or (find-file "entry.o" lib-dir)
          (throw 'error "Couldn't find entry file"))
      (join-str " " obj-files)
      (join-str
        " "
        (map (lambda (l)
               (cond ((starts-with "-l" l)
                      l)
                     ((not (ends-with ".a" l))
                      (concat "-l" l))
                     (else (find-file l lib-dir))))
             (unique (append libs
                             (get 'libs options))
                     string=?)))
      out)
    (if (file-exists? out)
      (when (get 'opt-strip options)
        (shell-exec
          options
          "strip {}" out))
      (throw 'error "Linker error"))))

(define (read-obj-dependencies obj-file options)
  (let ((s (dump-symbol obj-file "__depends")))
   (if s
     (unique (split-str "," s)
             string=?)
     (throw 'error "Error reading {}" obj-file))))

(define (compiler-args argv)
  (let* ((bin-path (file-directory (get-executable-path (car argv))))
         (lib-dirs
           (list (fmt #f "{}/../lib/cons/{}"
                      bin-path
                      default-arch)
                 (concat bin-path
                         "/../lib"))))
   (let+ (program-name . argv) argv
     `(("h" help flag "show this help")
       ("V" version flag "show version and exit")
       ("o" out single "output file")
       ("I" inc multiple "add include path"
        ,lib-dirs)
       ("L" lib-dir multiple "add library path"
        ,lib-dirs)
       ("linker" opt-ld single "specify linker program")
       ("dynamic-linker" opt-ld-so single "set dynamic linker")
       ("D" def multiple "add preprocessor definition")
       ("S" asm flag "generate assembly"
        #f (disjoint obj mod deps))
       ("c" obj flag "generate object file"
        #f (disjoint asm mod deps))
       ("m" mod flag "compile module"
        #f (disjoint asm obj deps))
       ("deps" deps flag "dump object file dependencies"
        #f (disjoint asm obj mod))
       ("g" opt-dbg flag "add debug symbols")
       ("v" verbose flag "verbose output")
       ("type-info" type-info flag "show type-checker info")
       ("env-info" env-info flag "show environment info")
       ("func-info" dump-funcs multiple "show type-checker info for given functions")
       ("l" libs multiple "link with library")
       ("a" arch single "specify architecture"
        ,default-arch)
       ("toplevel" toplevel-name single "toplevel procedure name")
       ("static" opt-static flag "statically link executable")
       ("Wl" ld-opts multiple "pass options to linker")
       ("no-pie" opt-pie neg-flag "disable PIE")
       ("strip" opt-strip flag "strip resulting binary")
       ("code-opt" opt-code-opt flag "enable peephole optimizations (experimental)")
       ("no-leaf" opt-leaf neg-flag "don't optimize leaf routines")
       ("" in multiple "input files")))))

(define (show-version)
  (fmt #t "{} bootstrapper v{}\nbuilt on: {}\nwith: {}\n"
       package-name
       package-version
       build-date
       compiled-with)
  (for-each (lambda (x)
              (fmt #t "    {}{}\n"
                   (ljust (concat (car x) ":") 16)
                   (cadr x)))
            `(("AS" ,as-program)
              ("LD" ,ld-program)
              ("CC" ,cc-program)
              ("AR" ,ar-program)
              ("dynamic linker" ,ld-so-path))))

(define (main argv)
  (define (categorize-input-files in)
    (foldr (lambda (x xs)
             (push-back (scase (file-suffix x)
                          (("s") 'asm-files)
                          (("o") 'obj-files)
                          (else 'src-files))
                        x
                        xs))
           '((src-files)
             (asm-files)
             (obj-files))
           in))
  (try
    (lambda ()
      (let ((options (set 'compiler-path
                          (get-executable-path (car argv))
                          (parse-argv (cdr argv)
                                      (compiler-args argv)))))
        (let+ (: help version asm obj mod deps in out) options
          (cond
            (help (show-help argv (compiler-args argv)))
            (version (show-version))
            (else
              (cond
                ((and (or asm obj mod)
                      (> (length in) 1)
                      out)
                 (throw
                   'error
                   "Can't specify output file with -S or -c with multiple input files"))
                ((null? in)
                 (throw
                   'error
                   "Input is empty"))
                (else
                  (let+
                    (: src-files asm-files obj-files) (categorize-input-files
                                                        in)
                    (cond
                      (asm (for-each (cut make-asm-from-src <> out options)
                                     src-files))
                      (obj (for-each (cut make-obj-from-src <> out options)
                                     src-files)
                           (for-each (cut make-obj-from-asm <> out options)
                                     asm-files))
                      (mod (dump-module (car in) out (set 'mod-name
                                                          (file-base-name
                                                            (drop-file-suffix
                                                              out))
                                                          options)))
                      (deps (for-each (lambda (x)
                                        (fmt #t " >> {}\n" x)
                                        (for-each (cut fmt #t "{}\n" <>)
                                                  (read-obj-dependencies x options)))
                                      in))
                      (else (let* ((tmp-files
                                     (append (map (cut make-obj-from-src <> #f options)
                                                  src-files)
                                             (map (cut make-obj-from-asm <> #f options)
                                                  asm-files)))
                                   (all-files (append tmp-files obj-files)))
                              (try
                                (lambda ()
                                  (build-executable
                                    all-files
                                    (unique (mappend (lambda (x)
                                                       (filter (! (cut string=? "" <>))
                                                               (read-obj-dependencies
                                                                 x options)))
                                                     all-files)
                                            string=?)
                                    (or out
                                        (if (> (length all-files) 1)
                                          "a.out"
                                          (drop-file-suffix (car all-files))))
                                    options))
                                (lambda (e) (throw e))
                                (lambda ()
                                  (for-each safe-delete tmp-files))))))))))))))
    (lambda (err)
      (fmt #t "Error({}): {}\n" (error-type err) (error-msg err))
      (exit 1))))
