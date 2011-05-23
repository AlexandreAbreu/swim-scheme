#lang racket

(require racket/string)

;; call w/ e.g. (dump-to-file "./cpp/scheme_func.asm" (lambda () (define-procs (lambda () (define-proc (lambda () (compile-program '(add1 42))))))))

;; string atom -> string

(define *function-name* "scheme_func")

(define *primitive-ops* '(add1 sub1 integer->char char->integer zero?))

(define pritimive-call?
  (lambda (element)
    (member element *primitive-ops*)
    )
  )

(define primitive-op
  (lambda (element)
    (car element)
    )
  )

;; 'n': zero based in the list of operands
;; list positive-integer -> atom
(define nth-primitive-call-operand
  (lambda (expression n)
    (list-ref expression (+ n 1))
    )
  )

(define first-primitive-call-operand
  (lambda (expression)
    (nth-primitive-call-operand expression 0)
    )
  )

;; list -> boolean
(define immediate-expression?
  (lambda (expression)
    (or (and (list? expression) (null? expression))
        (integer? expression)
        (boolean? expression)
        )
    )
  )

;; TODO macroify all the masks
(define fixnum-tag 0)
(define fixnum-shift 2)
(define fixnum-mask 3)
(define char-shift 8)
(define char-tag 15)
(define boolean-shift 7)
(define boolean-mask #b1111111)
(define boolean-tag #b0011111)
(define empty-list-value 47)

(define get-immediate-representation
  (lambda (element)
    (cond
      ((integer? element) (bitwise-ior (arithmetic-shift element fixnum-shift) fixnum-tag))
      ((boolean? element) (bitwise-ior (arithmetic-shift element char-shift) char-tag))
      ((char? element) (bitwise-ior (arithmetic-shift element boolean-shift) boolean-tag))
      ((null? element) empty-list-value)
      (else (error "Invalid element"))
      )
    )
  )

;; atom -> "tagged list"
(define make-immediate
  (lambda (value)
    `(:immediate ,(format "~s" (get-immediate-representation value)))
    )
  )

;; atom -> "tagged list"
(define make-registry
  (lambda (name)
    `(:registry ,name)
    )
  )

;; atom -> "tagged list"
(define make-instruction
  (lambda (name)
    `(:instruction ,name)
    )
  )

(define expression-tag
  (lambda (tagged-expression)
    (car tagged-expression)
    )
  )

(define expression-value
  (lambda (tagged-expression)
    (cadr tagged-expression)
    )
  )

;; TODO lots of boilerplate / repetition
;; tagged-expression symbol -> string
(define emit-code-for-expression
  (lambda (tagged-expression target)
    (define decorate-registry-windows
      (lambda (registry) registry)
      )
    (define decorate-immediate-windows
      (lambda (immediate-value) immediate-value)
      )
    (define decorate-instruction-windows
      (lambda (instruction) instruction)
      )
    ;; string -> string
    (define decorate-registry-linux
      (lambda (registry-name) (string-append "%" registry-name))
      )
    ;; string -> string
    (define decorate-immediate-linux
      (lambda (immediate-value-name) (if (fixnum? immediate-value-name) (string-append "$" immediate-value-name) immediate-value-name))
      )
    (define decorate-instruction-linux
      (lambda (instruction) instruction)
      )
    (case target
      ((x86-windows)
       (begin
         (case (expression-tag tagged-expression)
           ((:registry)
            (decorate-registry-windows (expression-value tagged-expression))
            )
           ((:immediate)
            (decorate-immediate-windows (expression-value tagged-expression))
            )
           ((:instruction)
            (decorate-instruction-windows (expression-value tagged-expression))
            )
           )
         )
       )
      ((x86-linux)
       (begin
         (case (expression-tag tagged-expression)
           ((:registry)
            (decorate-registry-linux (expression-value tagged-expression))
            )
           ((:immediate)
            (decorate-immediate-linux (expression-value tagged-expression))
            )
           ((:instruction)
            (decorate-instruction-linux (expression-value tagged-expression))
            )
           )
         )
       )
      )
    )
  )

(define emit-code-for-expressions
  (lambda (expressions target)
    (map (lambda (expression) (emit-code-for-expression expression target)) expressions)
    )
  )

(define eax "eax")

(define add "add")
(define sub "sub")
(define mov "mov")
(define shl "shl")
(define shr "shr")
(define and-inst "and")
(define or-inst "or")
(define cmp "cmp")
(define xor "xor")
(define sete "sete")
(define setne "sete")

(define emit
  (lambda (op-code . parameters)
    (display (string-append op-code " " (string-join parameters ",") "\n"))
    )
  )

(define emit1
  (lambda (f . a)
    (display (string-append
              (or (and (null? a) f)
                  (apply format (flatten `(,f ,a)))) "\n"))
    )
  )

(define emit-expression
  (lambda (expression target)
    (cond
      ((immediate-expression? expression)
       (apply emit (emit-code-for-expressions `(,(make-instruction mov) ,(make-registry eax) ,(make-immediate expression)) target))
       )
      ((pritimive-call? (primitive-op expression))
       (case (primitive-op expression)
         ((add1)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          ;; so we could end up w/ "'() + 1"
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction add) ,(make-registry eax) ,(make-immediate 1)) target))
          )
         ((sub1)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          ;; so we could end up w/ "'() - 1"
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction sub) ,(make-registry eax) ,(make-immediate 1)) target))
          )
         ((integer->char)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction shl) ,(make-registry eax) ,(make-immediate 6)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction and-inst) ,(make-registry eax) ,(make-immediate char-tag)) target))
          )
         ((char->integer)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction shr) ,(make-registry eax) ,(make-immediate 6)) target))
          )
         ((null?)
          ;; non check on validity of the value TODO
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction cmp) ,(make-registry eax) ,(make-immediate empty-list-value)) target))
          ;; TODO this is just to emit a boolean this should be refactored (since used all over)
          (apply emit (emit-code-for-expressions `(,(make-instruction xor) ,(make-registry eax) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction sete) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction shl) ,(make-registry eax) ,(make-immediate boolean-shift)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction or-inst) ,(make-registry eax) ,(make-immediate boolean-tag)) target))
          )
         ((integer?)
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction and-inst) ,(make-registry eax) ,(make-immediate fixnum-mask)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction cmp) ,(make-registry eax) ,(make-immediate fixnum-tag)) target))
          ;; TODO this is just to emit a boolean this should be refactored (since used all over)
          (apply emit (emit-code-for-expressions `(,(make-instruction xor) ,(make-registry eax) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction sete) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction shl) ,(make-registry eax) ,(make-immediate boolean-shift)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction or-inst) ,(make-registry eax) ,(make-immediate boolean-tag)) target))
          )
         ((boolean?)
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction and-inst) ,(make-registry eax) ,(make-immediate boolean-mask)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction cmp) ,(make-registry eax) ,(make-immediate boolean-tag)) target))
          ;; TODO this is just to emit a boolean this should be refactored (since used all over)
          (apply emit (emit-code-for-expressions `(,(make-instruction xor) ,(make-registry eax) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction sete) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction shl) ,(make-registry eax) ,(make-immediate boolean-shift)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction or-inst) ,(make-registry eax) ,(make-immediate boolean-tag)) target))
          )
         ((zero?)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction cmp) ,(make-registry eax) ,(make-immediate 0)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction xor) ,(make-registry eax) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction sete) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction shl) ,(make-registry eax) ,(make-immediate boolean-shift)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction or-inst) ,(make-registry eax) ,(make-immediate boolean-tag)) target))
          )
         ((not)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          ;; We assume that we have a boolean
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction shr) ,(make-registry eax) ,(make-immediate boolean-shift)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction cmp) ,(make-registry eax) ,(make-immediate 0)) target))
          ;; TODO this is just to emit a boolean this should be refactored (since used all over)
          (apply emit (emit-code-for-expressions `(,(make-instruction xor) ,(make-registry eax) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction setne) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction shl) ,(make-registry eax) ,(make-immediate boolean-shift)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction or-inst) ,(make-registry eax) ,(make-immediate boolean-tag)) target))
          )
         )
       )
      (else (error "invalid expression"))
      )
    )
  )

;; still very dummy, clumsy but slightly better

(define display-header-for-target
  (lambda (target)
    (let ([linux-header ("\t.text\n"
                         "\t.p2align 4,,15\n"
                         ".globl	scheme_func\n"
                         "\t.type	scheme_func, @function\n")]
          [windows-header (".686P\n"
                           ".XMM\n"
                           "include listing.inc\n"
                           ".model	flat\n"
                           "INCLUDELIB MSVCRTD\n"
                           "INCLUDELIB OLDNAMES\n"
                           "PUBLIC	_scheme_func\n")]
          )
      (case target
        ((x86-linux)
         (apply display linux-header)
         )
        ((x86-windows)
         (apply display windows-header)
         )
        )
      )
    )
  )

(define define-procs-for-target
  (lambda (procs target)
    (case target
      ((x86-linux)
       (display-header-for-target target)
       (procs)
       )
      ((x86-windows)
       (display-header-for-target target)
       (display "_TEXT	SEGMENT\n")
       (procs)
       (display "_TEXT	ENDS\n")
       (display "END\n")
       )
      )
    )
  )

(define define-proc-for-target
  (lambda (proc target)
    (case target
      ((x86-linux)
       (begin
         (display "scheme_func:\n")
         (proc)
         ;; should ret, but we do not mess w/ esp yet
         ;; (display "ret\n")
         )
       )
      ((x86-windows)
       (begin
         (display "_scheme_func	PROC\n")
         (proc)
         (display "ret 0\n")
         (display "_scheme_func	ENDP\n")
         )
       )
      )
    )
  )

(define compile-program
  (lambda (p target)
    (emit-expression p target)
    )
  )

(define dump-to-file
  (lambda (filename l)
    (with-output-to-file filename l #:exists 'replace)
    )
  )

