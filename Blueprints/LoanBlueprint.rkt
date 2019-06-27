(define-datatype Loan Loan?
  (an-loan 
    (amount integer?)
    (blocked-amount integer?)
    (return-span integer?)
    (interest integer?) ; Yearly
    (last-loan-span integer?)
    (minimum-credit integer?)
  )
)
