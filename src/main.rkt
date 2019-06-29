#lang racket

(require "../main.rkt")

(let ([file-path (read-line (current-input-port) 'any)])
  (let (input-data (analyse-input-file file-path))
    input-data
  )
)
