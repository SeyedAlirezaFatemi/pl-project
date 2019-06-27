(define-datatype Account Account?
  (an-account 
    (has-interest boolean?)
    (fee real?)
    (minimum-deposit real?) ; For opening new accounts. Checked after subtracting fee.
    (monthly boolean?) ; #t => monthly interest & #f => yearly interst
    ; Yearly interset is calculated based on the minimum amount of deposit in the year.
    (period integer?) ; After period months, there will be no more interest for this account.
    (renewable boolean?)
    (interest-rate real?) ; Yearly
    (credit real?)
    (has-variable-interest boolean?)
    (span-for-increase integer?)
    (increase-rate real?)
    (has-cheque boolean?)
    (has-card boolean?)
    (transfer-fee real?)     
  )
)
