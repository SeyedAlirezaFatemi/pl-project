#lang eopl

(provide (all-defined-out))

(define-datatype Loan Loan?
  (a-loan
    (id integer?)
    (amount integer?)
    (blocked-amount integer?)
    (return-span integer?)
    (interest integer?) ; Yearly
    (last-loan-span integer?)
    (minimum-credit integer?)
  )
)
