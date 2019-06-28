#lang eopl

(provide (all-defined-out))

(define-datatype Command Command?
  (time-command)
  (new-account-command
    (customer-id integer?)
    (account-type integer?)
    (inital-balance integer?)
  )
  (deposit-command
    (customer-id integer?)
    (amount integer?)
  )
  (renewal-command
    (customer-id integer?)  
  )
  (cheque-command
    (customer-id integer?)
    (amount integer?)        
  )
  (card-command
    (customer-id integer?)
    (amount integer?)
  )
  (transfer-command
    (customer-id integer?)
    (amount integer?)
  )
  (withdraw-command
    (customer-id integer?)
    (amount integer?)
  )
  (close-command
    (customer-id integer?)  
  )
  (pay-debt-command
    (customer-id integer?)
    (amount integer?)    
  )
  (withdraw-loan-command
    (customer-id integer?)
  )
)
