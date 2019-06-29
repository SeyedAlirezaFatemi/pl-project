#lang eopl

(provide (all-defined-out))

(require "./LoanState.rkt")

(define loan-states-list? (list? LoanState?))

(define-datatype Customer Customer?
  (a-customer
    (id integer?)
    (type integer?)
    (initial-amount integer?)
    (amount integer?)
    (deadline-month integer?)
    (credit-counter integer?)
    (credit integer?)
    (interest-rate integer?)
    (loans loan-states-list?)
    (minimum-amount integer?)
    (blocked-money integer?)
  )
)
