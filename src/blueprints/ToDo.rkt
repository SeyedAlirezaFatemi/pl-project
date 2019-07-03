#lang eopl

(provide (all-defined-out))
(require "../states/Customer.rkt")
(require "../blueprints/Loan.rkt")

(define-datatype ToDo ToDo?
  (give-loan
    (customer Customer?)
    (loan Loan?) 
  )
)