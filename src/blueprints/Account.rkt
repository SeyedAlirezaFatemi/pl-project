#lang eopl

(provide (all-defined-out))

(define-datatype Account Account?
  (an-account 
    (id integer?)
    (has-interest boolean?)
    (fee integer?)
    (minimum-deposit integer?) ; For opening new accounts. Checked after subtracting fee.
    (monthly boolean?) ; #t => monthly interest & #f => yearly interest
    ; Yearly interest is calculated based on the minimum amount of deposit in the year.
    (period integer?) ; After period months, there will be no more interest for this account.
    (renewable boolean?)
    (interest-rate integer?) ; Yearly
    (credit integer?)
    (has-variable-interest boolean?)
    (span-for-increase integer?)
    (increase-rate integer?)
    (has-cheque boolean?)
    (has-card boolean?)
    (transfer-fee integer?)     
  )
)

(define has-interest? 
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        has-interest
      )
    )
  ) 
)
