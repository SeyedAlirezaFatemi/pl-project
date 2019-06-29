#lang racket

(require "./constructor/constructor.rkt")
(require "./io/out.rkt")
(require "./commander/commander.rkt")

(display 'O_O)
(newline)

(let ([file-path (read-line (current-input-port) 'any)])
  (let ([input-data (analyse-input-file "samples/sample_input.txt")])
    (let* ([loan-types (car input-data)]
           [account-types (cadr input-data)]
           [commands (caddr input-data)])
      (let ([customers (work-on-commands loan-types account-types commands)])
        (
            begin
            (display 'hey!)
            (newline)
            (finish 4 customers)
        )
      )
    )
  )
)