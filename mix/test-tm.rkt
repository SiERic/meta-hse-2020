#lang racket

(require "tm.rkt")
(require "flowchart.rkt")
(require rackunit)

(define first-zero
  '((0 if 0 goto 3) 
    (1 right) 
    (2 goto 0) 
    (3 write 1)))

(check-equal? (fc-run tm-int `(,first-zero (1 1 1 0 1 0 1))) '(1 1 0 1))

(check-equal? (fc-run tm-int `(,first-zero (1 1 1 0))) '(1))

(check-equal? (fc-run tm-int `(,first-zero (0))) '(1))

(define write-5-zeros
  '((0 left)
    (1 write 0)
    (2 left)
    (3 write 0)
    (4 left)
    (5 write 0)
    (6 left)
    (7 write 0)
    (8 left)
    (9 write 0)))

(check-equal? (fc-run tm-int `(,write-5-zeros ())) '(0 0 0 0 0))

(check-equal? (fc-run tm-int `(,write-5-zeros (1 1))) '(0 0 0 0 0 1 1))
