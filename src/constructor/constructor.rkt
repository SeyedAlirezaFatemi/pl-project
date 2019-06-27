#lang racket

(provide loan-types)
(require "../commander/commander.rkt")

(define loan-types '())
(define account-types '())

(set! loan-types  (cons 2 loan-types))

(time-goes-by)
(write loan-types)
