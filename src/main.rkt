#lang racket

(require "./constructor/constructor.rkt")
(require "./io/out.rkt")

(let ([file-path (read-line (current-input-port) 'any)])
  (let ([input-data (analyse-input-file "samples/sample_input.txt")])
    (let* ([loan-types (car input-data)]
           [account-types (cadr input-data)]
           [commands (caddr input-data)])
      (list loan-types account-types commands)
      (finish 4 '())
    )
  )
)
