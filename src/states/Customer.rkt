#lang eopl

(provide (all-defined-out))

(require predicates)
(require "./LoanState.rkt")

(define loan-states-list? (all? LoanState?))

(define-datatype Customer Customer?
  (an-account-state
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
