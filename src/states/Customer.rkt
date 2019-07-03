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
  )
)

(define customer->id
  (lambda (customer)
    (cases Customer customer
      (a-customer (id type initial-amount amount 
                   deadline-month credit-counter credit 
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
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
                   interest-rate loans minimum-amount blocked-money)
        blocked-money
      )
    )
  )
)
