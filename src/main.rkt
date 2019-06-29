#lang racket

(require "./constructor/constructor.rkt")

(let ([file-path (read-line (current-input-port) 'any)])

  (let ([input-data (analyse-input-file "samples/sample_input.txt")])
    input-data
  )
)
