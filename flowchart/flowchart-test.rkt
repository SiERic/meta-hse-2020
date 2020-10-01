#lang racket

(require "flowchart.rkt")
(require rackunit)

(define get-value
  '((read value)
    (main (return value))
  ))

(check-equal? (flowchart-run get-value '(2)) '2)

(define find-name
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist  (cdr namelist))
          (goto search))
    (found (return (car valuelist)))
    ))

(check-equal? (flowchart-run find-name '(y (x y z) (1 2 3))) '2)

