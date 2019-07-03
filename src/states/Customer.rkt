#lang eopl

(provide (all-defined-out))
(require predicates)
(require "./LoanState.rkt")

(define loan-states-list? (all? LoanState?))

(define-datatype Customer Customer?
  (a-customer
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
    (creation-time integer?)
  )
)

(define customer->id
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        id
      )
    )
  )
)

(define customer->type
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        type
      )
    )
  )
)

(define customer->initial-amount
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        initial-amount
      )
    )
  )
)

(define customer->amount
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        amount
      )
    )
  )
)

(define customer->deadline-month
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        deadline-month
      )
    )
  )
)

(define customer->credit-counter
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        credit-counter
      )
    )
  )
)

(define customer->credit
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        credit
      )
    )
  )
)

(define customer->interest-rate
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        interest-rate
      )
    )
  )
)

(define customer->loans
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        loans
      )
    )
  )
)

(define customer->minimum-amount
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        minimum-amount
      )
    )
  )
)

(define customer->blocked-money
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money creation-time)
        blocked-money
      )
    )
  )
)
