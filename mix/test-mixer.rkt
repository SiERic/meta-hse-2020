#lang racket

(require "mixer.rkt")
(require "flowchart.rkt")
(require "tm.rkt")
(require rackunit)

(pretty-print-columns 100)

;; 0 Futamura
;; [| mix |] [source, input_st] := mixed_source
;; int [mixed_source, input_dn] -> output

(define program-example
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist  (cdr namelist))
          (goto search))
    (found (return (car valuelist)))))

(define division-example 
  '((name namelist) 
    (valuelist)))

(define vs0-example 
  '((name . y)
    (namelist . (x y z))))

(define mixed-program (fc-run mix `(,program-example ,division-example ,vs0-example)))
(check-equal? (fc-run mixed-program '((1 2 3))) '2)

(pretty-print mixed-program)

;; (pretty-print mixed-program) output:
; '((read valuelist) (search0 (:= valuelist (cdr valuelist)) (return (car valuelist))))

;; =====================================================================================

;; I Futamura

;; [| mix |] [int, source] =: target
;; int [target, input] -> output

(define first-zero
  '((0 if 0 goto 3) 
    (1 right) 
    (2 goto 0) 
    (3 write 1)))

(define tm-int-vs0
  `((q . ,first-zero)
    (qtail . '())
    (instruction . '())
    (operator . '())
    (nextlabel . '())))

(define tm-int-division
  '((q qtail instruction operator nextlabel)
    (left right symbol)))

(define target-fc (fc-run mix `(,tm-int ,tm-int-division ,tm-int-vs0)))
(check-equal? (fc-run target-fc '((1 1 1 0 1 0 1))) '(1 1 0 1))

(pretty-print target-fc)

; (pretty-print target-fc) output:

; '((read right)
;   (init0
;    (:= left '())
;    (:= symbol (caddr '(0 if 0 goto 3)))
;    (if (equal? symbol (car right)) jump0 loop0))
;   (jump0
;    (:= symbol (caddr '(3 write 1)))
;    (:= right (cons symbol (cdr right)))
;    (return (dropf-right right (lambda (x) (eq? x '2)))))
;   (loop0
;    (:= left (cons (car-or-space right) left))
;    (:= right (cdr-or-empty right))
;    (:= symbol (caddr '(0 if 0 goto 3)))
;    (if (equal? symbol (car right)) jump0 loop0)))

;; TODO: improve reduce-expr