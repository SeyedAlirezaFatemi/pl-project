#lang eopl

(provide (all-defined-out))

(define-datatype LoanState LoanState?
  (a-loan-state
    (time integer?)
    (type integer?)
    (debt integer?)
  )
)
