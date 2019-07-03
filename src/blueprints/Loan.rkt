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

(define loan->id
  (lambda (loan)
    (cases Loan loan
      (a-loan (id amount blocked-amount return-span interest last-loan-span minimum-credit) id)
    )
  )
)

(define loan->amount
  (lambda (loan)
    (cases Loan loan
      (a-loan (id amount blocked-amount return-span interest last-loan-span minimum-credit) amount)
    )
  )
)

(define loan->blocked-amount
  (lambda (loan)
    (cases Loan loan
      (a-loan (id amount blocked-amount return-span interest last-loan-span minimum-credit) blocked-amount)
    )
  )
)

(define loan->return-span
  (lambda (loan)
    (cases Loan loan
      (a-loan (id amount blocked-amount return-span interest last-loan-span minimum-credit) return-span)
    )
  )
)

(define loan->interest
  (lambda (loan)
    (cases Loan loan
      (a-loan (id amount blocked-amount return-span interest last-loan-span minimum-credit) interest)
    )
  )
)

(define loan->last-loan-span
  (lambda (loan)
    (cases Loan loan
      (a-loan (id amount blocked-amount return-span interest last-loan-span minimum-credit) last-loan-span)
    )
  )
)

(define loan->minimum-credit
  (lambda (loan)
    (cases Loan loan
      (a-loan (id amount blocked-amount return-span interest last-loan-span minimum-credit) minimum-credit)
    )
  )
)
