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

(define fixnum-shift 2)
(define char-shift 8)
(define char-tag 15)
(define boolean-shift 7)
(define boolean-tag #b0011111)
(define empty-list-value 47)

(define get-immediate-representation
  (lambda (element)
    (cond
      ((integer? element) (arithmetic-shift element fixnum-shift))
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
          (emit-expression (first-primitive-call-operand expression) target)
          (apply emit (emit-code-for-expressions `(,(make-instruction cmp) ,(make-registry eax) ,(make-immediate 0)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction xor) ,(make-registry eax) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction sete) ,(make-registry eax)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction shl) ,(make-registry eax) ,(make-immediate boolean-shift)) target))
          (apply emit (emit-code-for-expressions `(,(make-instruction or-inst) ,(make-registry eax) ,(make-immediate boolean-tag)) target))
          )
         )
       )
      (else (error "invalid expression"))
      )
    )
  )

(define compile-program
  (lambda (p)
    (emit-expression p 'x86-linux)
    )
  )

(define display-header-linux
  (lambda ()
    (display "\t.text\n")
    (display "\t.p2align 4,,15\n")
    (display ".globl	scheme_func\n")
    (display "\t.type	scheme_func, @function\n")
    )
  )

(define display-header
  (lambda ()
    (display ".686P\n")
    (display ".XMM\n")
    (display "include listing.inc\n")
    (display ".model	flat\n")
    (display "INCLUDELIB MSVCRTD\n")
    (display "INCLUDELIB OLDNAMES\n")
    (display "PUBLIC	_scheme_func\n")
    )
  )

(define define-procs-linux
  (lambda (procs)
    (begin
      (display-header)
      (procs)
      )
    )
  )

(define define-proc-linux
  (lambda (proc)
    (begin
      (display "scheme_func:\n")
      (proc)
      )
    )
  )

(define define-procs
  (lambda (procs)
    (begin
      (display-header)
      (display "_TEXT	SEGMENT\n")
      (procs)
      (display "_TEXT	ENDS\n")
      (display "END\n")
      )
    )
  )

(define define-proc
  (lambda (proc)
    (begin
      (display "_scheme_func	PROC\n")
      (proc)
      (display "ret 0\n")
      (display "_scheme_func	ENDP\n")
      )
    )
  )

(define dump-to-file
  (lambda (filename l)
    (with-output-to-file filename l #:exists 'replace)
    )
  )

