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

(define account->id 
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        id
      )
    )
  ) 
)

(define account->has-interest
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

(define account->fee
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        fee
      )
    )
  )
)

(define account->minimum-deposit
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        minimum-deposit
      )
    )
  ) 
)

(define account->monthly
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        monthly
      )
    )
  ) 
)

(define account->period
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        period
      )
    )
  ) 
)

(define account->renewable
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        renewable
      )
    )
  ) 
)

(define account->interest-rate
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        interest-rate
      )
    )
  ) 
)

(define account->credit
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        credit
      )
    )
  ) 
)

(define account->has-variable-interest
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        has-variable-interest
      )
    )
  ) 
)

(define account->span-for-increase
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        span-for-increase
      )
    )
  ) 
)

(define account->increase-rate
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        increase-rate
      )
    )
  ) 
)

(define account->has-cheque
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        has-cheque
      )
    )
  ) 
)

(define account->has-card
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        has-card
      )
    )
  ) 
)

(define account->transfer-fee
  (lambda (account-type)
    (cases Account account-type
      (an-account  (id has-interest fee minimum-deposit monthly
                    period renewable interest-rate credit has-variable-interest
                    span-for-increase increase-rate has-cheque has-card transfer-fee)
        transfer-fee
      )
    )
  ) 
)
