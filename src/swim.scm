#lang racket

;; call w/ e.g. (dump-to-file "./cpp/scheme_func.asm" (lambda () (define-procs (lambda () (define-proc (lambda () (compile-program '(add1 42))))))))

;; string atom -> string
(define *function-name* "scheme_func")

(define *primitive-ops* '(add1 sub1))

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
(define boolean-tag 31)
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

(define emit-expression
  (lambda (expression)
    (cond
      ((immediate-expression? expression)
       (emit "mov eax, ~s" (get-immediate-representation expression))
       )
      ((pritimive-call? (primitive-op expression))
       (case (primitive-op expression)
         ((add1)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          ;; so we could end up w/ "'() + 1"
          (emit-expression (first-primitive-call-operand expression))
          (emit "add eax, ~s" (get-immediate-representation 1))
          )
         ((sub1)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          ;; so we could end up w/ "'() - 1"
          (emit-expression (first-primitive-call-operand expression))
          (emit "sub eax, ~s" (get-immediate-representation 1))
          )
         ((integer->char)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          (emit-expression (first-primitive-call-operand expression))
          (emit "shl eax, 6")
          (emit "and eax, ~s" (define char-tag 15))
          )
         ((char->integer)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          (emit-expression (first-primitive-call-operand expression))
          (emit "shr eax, 6")
          )
         ((zero?)
          ;; based on eax only
          ;; move value -> eax, no type validation!!
          (emit-expression (first-primitive-call-operand expression))
          (emit "shr eax, 6")
          )
        ))
      )
    )
  )
       
(define emit
  (lambda (f . a)
    (display (string-append
              (or (and (null? a) f)
                  (apply format (flatten `(,f ,a)))) "\n"))
    )
  )

(define compile-program
  (lambda (p)
    (emit-expression p)
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

