#lang racket

(provide (all-defined-out))
(require racket/lazy-require)
(lazy-require ["../constructor/constructor.rkt" (loan-types)])

(define time-goes-by
  (lambda ()
    (display (cons 'a loan-types))
  )
)

; (define )