#lang racket

(require "./constructor/constructor.rkt")
(require "./io/out.rkt")
(require "./commander/commander.rkt")
(require racket/pretty)

(let ([file-path (read-line (current-input-port) 'any)])
  (let ([input-data (analyse-input-file "samples/sample_input-2.txt")])
    (pretty-display "File read successfully!")
    (let* ([loan-types (car input-data)]
           [account-types (cadr input-data)]
           [commands (caddr input-data)])
      (pretty-display "Loan Types:")
      (pretty-display loan-types)
      (pretty-display "Account Types:")
      (pretty-display account-types)
      (pretty-display "Commands:")
      (pretty-display commands)
      (let ([customers (work-on-commands loan-types account-types commands)])
        (
          begin
          (pretty-display customers)
          (finish 4 account-types customers)
        )
      )
    )
  )
)
