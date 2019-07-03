#lang racket

(provide (all-defined-out))
(require eopl)
(require racket/pretty)
(require "../io/in.rkt")
(require "../commander/Command.rkt")
(require "../states/Customer.rkt")
(require "../states/LoanState.rkt")
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

(define log
  (lambda (before current-command current-customers current-tasks current-month-number)
    (begin
      (pretty-display "**********")
      (if before
        (pretty-display "Before: ")
        (pretty-display "After: ")
      )
      (pretty-display current-command)
      (pretty-display "Customers: ")
      (pretty-display current-customers)
      (pretty-display "Tasks: ")
      (pretty-display current-tasks)
      (pretty-display "**********")
    )
  )
)

(define get-account-type
  (lambda (search-id account-type-list)
    (if (null? account-type-list)
      (raise 'account-not-found)
      (let ([account (car account-type-list)])
        (let ([account-id (account->id account)])
          (if (= account-id search-id)
            account
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

(define modify-loan
  (lambda (loan loan-list)
    (if (null? loan-list)
      (raise 'loan-not-found-for-save)
      (cases LoanState loan
        (a-loan-state (time type debt is-withdrawn)
          (let ([head (car loan-list)])
            (cases LoanState head
              (a-loan-state (head-time head-type head-debt head-is-withdrawn)
                (if (= head-time time)
                  (cons loan (cdr loan-list))
                  (cons head (modify-loan loan (cdr loan-list)))
                )
              )
            )
          )
        )
      )
    )
  )
)

(define save-loan
  (lambda (loan customer customers)
    (cases Customer customer
      (a-customer (id type initial-amount amount
                  deadline-month credit-counter credit
                  interest-rate loans minimum-amount blocked-money creation-time)
        (let [new-customer (a-customer (id type initial-amount amount
                                        deadline-month credit-counter credit
                                        interest-rate (modify-loan loan loans) minimum-amount blocked-money creation-time))]
          (save-customer new-customer)
        ) 
      )
    )
  )
)

(define get-customer
  (lambda (search-id customer-list)
    (if (null? customer-list)
      (raise 'customer-not-found)
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
                      interest-rate loans minimum-amount blocked-money creation-time)
          (let ([head (car customer-list)])
            (cases Customer head
              (a-customer (head-id head-type head-initial-amount head-amount
                            head-deadline-month head-credit-counter head-credit
                            head-interest-rate head-loans head-minimum-amount head-blocked-money creation-time)
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
                  interest-rate loans minimum-amount blocked-money creation-time)
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
                      interest-rate loans minimum-amount blocked-money month-number
                  )])
                (begin
                  (save-customer modified-customer)
                  (pretty-display "Punishing customer #")
                  (pretty-display customer-id)
                  (newline)
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
                           interest-rate loans minimum-amount blocked-money creation-time)
                (save-customer (an-account id type initial-amount (+ (loan->amount loan) current-amount) 
                                           deadline-month credit-counter (- credit (loan->minimum-credit loan))
                                           interest-rate (append loans (list (a-loan-state month-number (loan->id loan) (loan->amount loan) #f))) 
                                           minimum-amount (+ blocked-money (loan->blocked-amount loan))))
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

(define calculate-interest
  (lambda (interest-rate amount monthly)
    (let ([interest (* amount interest-rate)])
      (if monthly
        (exact-floor (/ interest 1200))
        (exact-floor (/ interest 100))
      )
    )
  )
)

(define pay-interests
  (lambda (month-number customers)
    (if (null? customers)
      #t ; Done
      (let ([current-customer (car customers)])
        (cases Customer current-customer
          (a-customer (id type initial-amount current-amount
                       deadline-month credit-counter credit
                       interest-rate loans minimum-amount blocked-money creation-time)
            (let* ([interest (calculate-interest interest-rate minimum-amount (account->monthly (get-account-type type)))]
                   [monthly (account->monthly (get-account-type type))])
              (if monthly
                (save-customer (a-customer (id type initial-amount (+ interest current-amount)
                                            deadline-month credit-counter credit
                                            interest-rate loans (+ interest current-amount) blocked-money creation-time)))
                (if (= 0 (modulo (- month-number creation-time) 12))
                  (begin
                    (save-customer (a-customer (id type initial-amount (+ interest current-amount)
                                                deadline-month credit-counter credit
                                                interest-rate loans (+ interest current-amount) blocked-money creation-time)))
                    ; Log
                    (pretty-display "^^^^^^^^^^")
                    (pretty-display "Interest time:")
                    (pretty-display current-customer)
                    (pretty-display interest)
                    (pretty-display "^^^^^^^^^^")
                  )
                  #t ; Done  
                )
              )
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
            ; Update minimum amount of customers based on monthly or yearly
            (pay-interests month-number customers)
            (do-tasks tasks)
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
                                    (- initial-balance fee)         ; => minimum-amount
                                    0                               ; => blocked-money
                                    month-number
                        )])
                  (begin
                    (set! customers (cons new-customer customers))
                    ; log
                    (pretty-display "New customer:")
                    (pretty-display new-customer)
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
                            interest-rate loans minimum-amount blocked-money creation-time)
                (let ([modified-customer
                    (a-customer id type initial-amount
                        (+ amount add-amount)
                        deadline-month credit-counter credit
                        interest-rate loans minimum-amount blocked-money creation-time
                    )])
                  (begin
                    (save-customer modified-customer)
                    ; Log
                    (pretty-display "##########")
                    (pretty-display "New Deposit:")
                    (display add-amount)
                    (display "$ added to the account of customer #")
                    (display customer-id)
                    (newline)
                    (pretty-display "##########")
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
                          interest-rate loans minimum-amount blocked-money creation-time)
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
                              (save-customer modified-customer)
                              (pretty-display "!!!!!!!!!!")
                              (pretty-display "Renewal:")
                              (display "We have the renewal of customer #")
                              (display customer-id)
                              (newline)
                              (pretty-display "!!!!!!!!!!")
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
                          interest-rate loans minimum-amount blocked-money creation-time)
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
                              (save-customer modified-customer)
                              (pretty-display "&&&&&&&&&&")
                              (pretty-display "Cheque:")
                              (display cheque-amount)
                              (display "$ is paid by cheque by customer #")
                              (display customer-id)
                              (newline)
                              (pretty-display "&&&&&&&&&&")
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
                          interest-rate loans minimum-amount blocked-money creation-time)
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
                                        interest-rate loans minimum-amount blocked-money creation-time
                            )])
                              (begin
                                (save-customer modified-customer)
                                (pretty-display "~~~~~~~~~~")
                                (pretty-display "Card:")
                                (display card-amount)
                                (display "$ is paid by card by customer #")
                                (display customer-id)
                                (newline)
                                (pretty-display "~~~~~~~~~~")
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
                          interest-rate loans minimum-amount blocked-money creation-time)
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
                                        interest-rate loans minimum-amount blocked-money creation-time
                            )])
                            (begin
                              (save-customer modified-customer)
                              (pretty-display "->->->->->")
                              (pretty-display "Transfer:")
                              (display transfer-amount)
                              (display "$ is paid by transfer command by customer #")
                              (display customer-id)
                              (newline)
                              (pretty-display "->->->->->")
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
                          interest-rate loans minimum-amount blocked-money creation-time)
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
                                        interest-rate loans minimum-amount blocked-money creation-time
                            )])
                            (begin
                              (save-customer modified-customer)
                              (pretty-display "$$$$$$$$$$")
                              (pretty-display "Withdrawal:")
                              (display withdraw-amount)
                              (display "$ is paid by withdraw command by customer #")
                              (display customer-id)
                              (newline)
                              (pretty-display "$$$$$$$$$$")
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
                  (a-customer (id type initial-amount account-amount
                              deadline-month credit-counter credit
                              interest-rate loans minimum-amount blocked-money creation-time)
                    (let ([modified-customer
                      (a-customer id type initial-amount
                        (- account-amount amount)
                        deadline-month
                        credit-counter credit
                        interest-rate loans minimum-amount blocked-money creation-time
                      )])
                      (let ([latest-loan (latest-loan loans)])
                        (cases LoanState latest-loan
                          (a-loan-state (time type debt is-withdrawn)
                            (if (>= debt amount)
                              (let* ([modified-loan
                                (a-loan-state time type (- debt amount) is-withdrawn)]
                                [extra (- amount debt)]
                                [modified-loan-2
                                (a-loan-state time type 0 is-withdrawn)]
                                [modified-customer-2
                                (a-customer id type initial-amount (+ amount extra)
                                deadline-month credit-counter credit
                                interest-rate loans minimum-amount blocked-money creation-time)]
                                )
                                (begin
                                  (save-customer modified-customer customers)
                                  (save-loan modified-loan modified-customer customers)
                                )
                                (begin
                                  (save-customer modified-customer-2 customers)
                                  (save-loan modified-loan-2 modified-customer-2 customers)
                                )
                              )
                              ; TODO @estri
                              3
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
        (withdraw-loan-command (customer-id)
          (let ([customer (get-customer customer-id customers)])
            (let ([loans (customer->loans customer)])
              (if (null? loans)
                ; No loan to withdraw
                (begin
                  (pretty-display "Customer has no loan to withdraw.")
                  (pretty-display command)
                )
                ; Find a suitable loan to withdraw
                (let ([found (find-first-not-withdrawn-loan loans)])
                  (if found
                    (let ([loan-type (get-loan-type (loan-state->type found) loan-types)])
                      (cases Customer customer
                        (a-customer (id type initial-amount amount
                                    deadline-month credit-counter credit
                                    interest-rate loans minimum-amount blocked-money creation-time)
                          (save-customer (an-account id type initial-amount (- amount (loan->amount))
                                                     deadline-month credit-counter credit
                                                     interest-rate (flip-first-not-withdrawn-loan loans) minimum-amount blocked-money creation-time))
                        )
                      )
                    )
                    ; All loans are withdrawn
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
        (log #t (car commands) customers tasks month-number)
        (do-command (car commands))
        (log #f (car commands) customers tasks month-number)
        (do-commands (cdr commands))
      )
    )
  )
)

(define work-on-commands
  (lambda (ls as cs)
    (begin
      (pretty-display "@@@@@@@@@@")
      (pretty-display "Processing commands...")
      (set! account-types as)
      (set! loan-types ls)
      (set! commands cs)
      (do-commands commands)
    )
  )
)
