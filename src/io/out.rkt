#lang eopl


(require "../states/Customer.rkt")
(provide (all-defined-out))

(define finish
  (lambda (last-month customers)
    (let ([out (open-output-file "result.txt")])
      (write last-month out)
      (newline out)
      (write-customers customers out)
      (close-output-port out)
    )
  ) 
)

(define write-customers
  (lambda (customers out)
    (if (null? customers) 
      out
      (cases Customer (car customers)
          (an-account-state (id type initial-amount amount deadline-month credit-counter credit interest-rate loans minimum-amount blocked-money)
            (write id out)
            (write initial-amount out)

            (write-customers (cdr customers) out)
          )
      )
    )
  )
)
