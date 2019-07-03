#lang racket

(provide (all-defined-out))
(require eopl)
(require racket/pretty)
(require "../io/in.rkt")
(require "../commander/Command.rkt")
(require "../states/Customer.rkt")
(require "../blueprints/Account.rkt")
(require "../blueprints/Loan.rkt")
(require "../blueprints/Task.rkt")
(require "../utils/helpers.rkt")

(define loan-types '())
(define account-types '())
(define commands '())
(define customers '())
(define tasks '())
(define month-number 0)

(define get-account-type
  (lambda (search-id account-type-list)
    (if (null? account-type-list)
      (begin (raise 'account-not-found))
      (let ([acc (car account-type-list)])
        (let ([acc-id (account->id acc)])
          (if (= acc-id search-id)
            acc
            (get-account-type search-id (cdr account-type-list))
          )
        )
      )
    )
  )
)

(define get-loan-type
  (lambda (search-id loan-type-list)
    (if (null? loan-type-list)
      (raise 'loan-not-found)
      (let ([loan (car loan-type-list)])
        (let ([loan-id (loan->id loan)])
          (if (= loan-id search-id)
            loan
            (get-loan-type search-id (cdr loan-type-list))
          )
        )
      )
    )
  )
)

(define get-customer
  (lambda (search-id customer-list)
    (if (null? customer-list)
      (begin (raise 'customer-not-found))
      (let ([customer (car customer-list)])
        (let ([customer-id (customer->id customer)])
          (if (= customer-id search-id)
            customer
            (get-customer search-id (cdr customer-list))
          )
        )
      )
    )
  )
)

(define get-customers-account
  (lambda (customer)
    (get-account-type (customer->type customer) account-types)
  )
)

(define modify-customer
  (lambda (customer customer-list)
    (if (null? customer-list)
      (raise 'customer-not-found-for-save)
      (cases Customer customer
        (a-customer (id type initial-amount amount
                      deadline-month credit-counter credit
                      interest-rate loans minimum-amount blocked-money)
          (let ([head (car customer-list)])
            (cases Customer head
              (a-customer (head-id head-type head-initial-amount head-amount
                            head-deadline-month head-credit-counter head-credit
                            head-interest-rate head-loans head-minimum-amount head-blocked-money)
                (if (= head-id id)
                  (cons customer (cdr customer-list))
                  (cons head (modify-customer customer (cdr customer-list)))
                )
              )
            )
          )
        )
      )
    )
  )
)

(define delete-customer
  (lambda (search-id customer-list)
    (if (null? customer-list)
      (raise 'customer-not-found-for-deletion)
      (let ([head (car customer-list)])
        (let ([head-id (customer->id head)])
          (if (= head-id search-id)
            (cdr customer-list)
            (cons head (delete-customer search-id (cdr customer-list)))
          )
        )
      )
    )
  )
)

(define save-customer
  (lambda (customer)
    (begin
      (set! customers (modify-customer customer customers))
    )
  )
)

(define punish
  (lambda (customer account)
    (cases Customer customer
      (a-customer (customer-id type initial-amount amount
                  deadline-month credit-counter credit
                  interest-rate loans minimum-amount blocked-money)
        (let ([account (get-customers-account customer)])
          (cases Account account
            (an-account  (id has-interest fee minimum-deposit monthly
                          period renewable interest-rate account-credit has-variable-interest
                          span-for-increase increase-rate has-cheque has-card transfer-fee)
              (let ([modified-customer
                  (a-customer id type initial-amount
                      amount
                      deadline-month
                      0
                      (- credit (/ account-credit 2))
                      interest-rate loans minimum-amount blocked-money
                  )])
                (begin
                  (save-customer modified-customer)                   ; LOG
                  (pretty-display "Punishing customer #")                    ; LOG
                  (pretty-display customer-id)                               ; LOG
                  (newline)                                           ; LOG
                )
              )
            )
          )
        )
      )
    )
  )
)

(define create-loan
  (lambda (customer loan)
    (let ([amount (loan->amount loan)])
      (add-task (give-loan-task customer loan))
    )
  )
)

(define add-task
  (lambda (new-task)
    (set! tasks (cons new-task tasks))
    (pretty-display "New task added.")
    (pretty-display new-task)
  )
)

(define do-tasks
  (lambda (current-tasks)
    (if (null? current-tasks)
      (set! tasks '()) ; We're done
      (let ([current-task (car current-tasks)])
        (cases Task current-task
          (give-loan-task (customer loan) 
            (cases Customer customer
              (a-customer (id type initial-amount current-amount 
                           deadline-month credit-counter credit 
                           interest-rate loans minimum-amount blocked-money)
                (save-customer (an-account id type initial-amount (+ (loan->amount loan) current-amount) 
                                           deadline-month credit-counter (- credit (loan->minimum-credit loan))
                                           interest-rate (append loans (list (a-loan-state month-number (loan->id loan) (loan->amount loan) #f))) 
                                           minimum-amount (+ blocked-money (loan->blocked-amount loan))) customers)
              )
            )
          )
        )
      )
    )
  )
)

(define find-first-not-withdrawn-loan
  (lambda (loans)
    (if (null? loans)
      #f
      (let ([last-loan (car loans)])
        (if (loan-state->is-withdrawn last-loan)
          (find-first-not-withdrawn-loan (cdr loans))
          last-loan
        )
      )
    )
  )
)

(define flip-first-not-withdrawn-loan
  (lambda (loans)
    (if (null? loans)
      loans
      (let ([last-loan (car loans)])
        (if (loan-state->is-withdrawn last-loan)
          (cons last-loan (find-first-not-withdrawn-loan (cdr loans)))
          (cases LoanState last-loan
            (a-loan-state (time type debt is-withdrawn)
              (cons (a-loan-state time type debt #t) (cdr loans))
            )
          )
        )
      )
    )
  )
)

(define do-command
  (lambda (command)
    (with-handlers ([symbol? (lambda (exn) (begin (pretty-display "Exception: ") (pretty-display exn) (newline)))])     ; LOG
      (cases Command command
        (time-command ()
          (begin
            (do-tasks)
            (set! month-number (+ month-number 1))
          )
        )
        (new-account-command (customer-id account-type initial-balance)
          (let ([acc (get-account-type account-type account-types) ])
            (cases Account acc 
              (an-account (id has-interest
                           fee minimum-deposit monthly period
                           renewable interest-rate credit
                           has-variable-interest span-for-increase
                           increase-rate has-cheque has-card transfer-fee)
                (let ([new-customer
                      (a-customer customer-id account-type
                      (- initial-balance fee)         ; => initial-amount
                      (- initial-balance fee)         ; => amount
                      (+ month-number period)         ; => deadline-month
                      0                               ; => credit-counter
                      0                               ; => credit
                      interest-rate
                      '()                             ; => loans
                      minimum-deposit
                      0                               ; => blocked-money
                      )])
                  (begin
                    (set! customers (cons new-customer customers))
                    (pretty-display 'account-created!) ; LOG
                    (newline)                   ; LOG
                  )
                )
              )
            )
          )
        )
        (deposit-command (customer-id add-amount)
          (let ([customer (get-customer customer-id customers)])
            (cases Customer customer
              (a-customer (id type initial-amount amount
                            deadline-month credit-counter credit
                            interest-rate loans minimum-amount blocked-money)
                (let ([modified-customer
                    (a-customer id type initial-amount
                        (+ amount add-amount)
                        deadline-month credit-counter credit
                        interest-rate loans minimum-amount blocked-money
                    )])
                  (begin
                    (save-customer modified-customer)
                    (pretty-display add-amount)                                ; LOG
                    (pretty-display "$ added to the account of customer #")    ; LOG
                    (pretty-display customer-id)                               ; LOG
                    (newline)                                           ; LOG
                  )
                )
              )
            )
          )
        )
        (renewal-command (customer-id)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (if (>= deadline-month month-number)
                            (raise 'wait-for-the-deadline)
                            (let ([account (get-customers-account customer)])
                               (cases Account account
                                 (an-account  (id has-interest fee minimum-deposit monthly
                                                period renewable interest-rate credit has-variable-interest
                                                span-for-increase increase-rate has-cheque has-card transfer-fee)
                                    (if renewable
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                amount
                                                (+ month-number period)
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                              ; LOG
                                            (pretty-display "We have the renewal of customer #")       ; LOG
                                            (pretty-display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-renewable)
                                    )
                                 )
                               )
                            )
                        )
                    )
                )
            )
        )
        (cheque-command (customer-id cheque-amount)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-cheque
                                    (if (>= (- amount cheque-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount cheque-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                   ; LOG
                                            (pretty-display cheque-amount)                             ; LOG
                                            (pretty-display "$ is paid by cheque by customer #")       ; LOG
                                            (pretty-display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-cheque)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-chequeable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (card-command (customer-id card-amount)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-card
                                    (if (>= (- amount card-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount card-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                   ; LOG
                                            (pretty-display card-amount)                               ; LOG
                                            (pretty-display "$ is paid by card by customer #")         ; LOG
                                            (pretty-display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-card)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-cardable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (transfer-command (customer-id transfer-amount) 7
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-card
                                    (if (>= (- amount transfer-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount transfer-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                                ; LOG
                                            (pretty-display transfer-amount)                                        ; LOG
                                            (pretty-display "$ is paid by transfer command by customer #")          ; LOG
                                            (pretty-display customer-id)                                            ; LOG
                                            (newline)                                                        ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-transfer)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-transferable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (withdraw-command (customer-id withdraw-amount)
            (let ([customer (get-customer customer-id customers)])
                (cases Customer customer
                    (a-customer (id type initial-amount amount
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money)
                        (let ([account (get-customers-account customer)])
                           (cases Account account
                             (an-account  (id has-interest fee minimum-deposit monthly
                                            period renewable interest-rate credit has-variable-interest
                                            span-for-increase increase-rate has-cheque has-card transfer-fee)
                                (if has-card
                                    (if (>= (- amount withdraw-amount) minimum-deposit)
                                        (let ([modified-customer
                                            (a-customer id type initial-amount
                                                (- amount withdraw-amount)
                                                deadline-month
                                                credit-counter credit
                                                interest-rate loans minimum-amount blocked-money
                                            )])
                                          (begin
                                            (save-customer modified-customer)                   ; LOG
                                            (pretty-display withdraw-amount)                               ; LOG
                                            (pretty-display "$ is paid by withdraw command by customer #")         ; LOG
                                            (pretty-display customer-id)                               ; LOG
                                            (newline)                                           ; LOG
                                          )
                                        )
                                        (raise 'not-enough-money-for-withdraw)
                                    )
                                    (begin
                                        (punish customer account)
                                        (raise 'not-withdrawable!)
                                    )
                                )
                             )
                           )
                        )
                    )
                )
            )
        )
        (close-command (customer-id)
            (set! customers (delete-customer customer-id customers))
        )
        (request-loan-command (customer-id loan-type) 
          (let* ([customer (get-customer customer-id customers)]
                 [loan (get-loan-type loan-type)])
            (if (and 
                  (>= (customer->amount customer) (loan->blocked-amount loan))
                  (>= (customer->credit customer) (loan->minimum-credit loan))
                )
              (if (latest-loan-time (customer->loans customer))
                (if (> month-number (+ (latest-loan-time (customer->loans customer)) (loan->last-loan-span loan)))
                  (create-loan customer loan)
                  (begin
                    (pretty-display "Request for loan denied.")
                    (pretty-display command)
                  )
                ) 
                (create-loan customer loan)
              )
              (begin
                (pretty-display "Request for loan denied.")
                (pretty-display command)
              )
            )
          )
        )
        (pay-debt-command (customer-id amount)
          (let* ([customer (get-customer customer-id customers)]
                 [account-amount (customer->amount customer)])
            (if (> amount account-amount)
              (pretty-display "You don't have enoygh money in your account.")
              (begin
                (cases Customer customer
                  (a-customer (id type initial-amount amount
                              deadline-month credit-counter credit
                              interest-rate loans minimum-amount blocked-money)
                    (let ([modified-customer
                      (a-customer id type initial-amount
                        (- account-amount amount)
                        deadline-month
                        credit-counter credit
                        interest-rate loans minimum-amount blocked-money
                      )])
                      (let* ([latest-time (latest-loan-time loans)]
                            [latest-debt (time->debt latest-time loans)])
                        (if (> debt )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (withdraw-loan-command (customer-id)
          (let ([customer (get-customer customer-id customers)])
            (let ([loans (customer->loans customer)])
              (if (null? loans)
                (begin
                  (pretty-display "Customer has no loan to withdraw.")
                  (pretty-display command)
                )
                (let ([found (find-first-not-withdrawn-loan loans)])
                  (if found
                    (let ([loan-type (get-loan-type (loan-state->type found) loan-types)])
                      ; Update customer and its loans
                    )
                    (begin 
                      (pretty-display "Nothing to withdraw!")
                      (pretty-display command)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(define do-commands
  (lambda (commands)
    (if (null? commands)
      customers
      (begin
        (do-command (car commands))
        (pretty-display month-number)                   ; LOG
        (newline)                                       ; LOG
        (pretty-display customers)                      ; LOG
        (newline)                                       ; LOG
        (do-commands (cdr commands))
      )
    )
  )
)

(define work-on-commands
  (lambda (ls as cs)
    (begin
      (pretty-display 'Started!!)                      ; LOG
      (newline)                                        ; LOG
      (set! account-types as)
      (set! loan-types ls)
      (set! commands cs)
      (do-commands commands)
    )
  )
)
