#lang eopl

(provide (all-defined-out))
(require "../states/Customer.rkt")
(require "../blueprints/Loan.rkt")

(define-datatype Task Task?
  (give-loan-task
    (customer Customer?)
    (loan Loan?) 
  )
)