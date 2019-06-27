#lang eopl

(provide (all-defined-out))

(define-datatype Command Command?
  (time)
  (new-account 
    (customer-id integer?)
    (account-type integer?)
    (inital-balance integer?)
  )
  (deposit 
    (customer-id integer?)
    (amount integer?)
  )
  (renewal
    (customer-id integer?)  
  )
  (cheque
    (customer-id integer?)
    (amount integer?)        
  )
  (card
    (customer-id integer?)
    (amount integer?)
  )
  (transfer
    (customer-id integer?)
    (amount integer?)
  )
  (withdraw
    (customer-id integer?)
    (amount integer?)
  )
  (close
    (customer-id integer?)  
  )

)
