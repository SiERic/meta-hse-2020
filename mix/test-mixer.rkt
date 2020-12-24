#lang racket

(require "mixer.rkt")
(require "flowchart.rkt")
(require "flowchart-flowchart.rkt")
(require "tm.rkt")
(require rackunit)

(pretty-print-columns 100)

;; 0 Futamura
;; [| mix |] [source, input_st] := mixed_source
;; int [mixed_source, input_dn] -> output

(display "\n ==  0 FUTAMURA == \n")

(display "0 Futamura\n\n")
(flush-output)

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

;; =====================================================================================

;; I Futamura

(display "\n ==  I FUTAMURA == \n")

;; [| mix |] [int, source] =: target
;; int [target, input] -> output

;; ------------- for TM
(display "I Futamura for TM\n")
(flush-output)

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

(define tm-target (fc-run mix `(,tm-int ,tm-int-division ,tm-int-vs0)))
(check-equal? (fc-run tm-target '((1 1 1 0 1 0 1))) '(1 1 0 1))

(pretty-print tm-target)

;; ------------- for FC
(display "I Futamura for FC\n")
(flush-output)

(define fc-int-vs0
  `((program . ,program-example)
    (data-vars . '())
    (bb-ns . '())
    (bb . '())
    (command . '())))

(define fc-int-division
  `((program data-vars bb-ns bb command)
    (ns)))

(define fc-target (fc-run mix `(,fc-int ,fc-int-division ,fc-int-vs0)))
(check-equal? (fc-run fc-target '((y (x y z) (1 2 3)))) '2)

(pretty-print fc-target)

;; =====================================================================================

;; II Futamura

(display "\n ==  II FUTAMURA == \n")

;; [| mix |] [mix, int] =: comp
;; int [comp, source] -> target
;; int [target, input] -> output

(define mix-division
  '((program division blocks-in-pending pp bb command x expr pp-then pp-else)
    (vs0 pending marked residual ppp vs code)))

;; ------------- for TM
(display "\nII Futamura for TM\n\n")
(flush-output)

(define mix-tm-int-vs0
  `((program . ,tm-int)
    (division . ,tm-int-division)
    (blocks-in-pending . '())
    (pp . '())
    (command . '())))

(define tm-compiler (fc-run mix `(,mix ,mix-division ,mix-tm-int-vs0)))
(display "  TM -> FC compiler:\n")
; (pretty-print tm-compiler)

(define compiled-tm-target (fc-run tm-compiler `(,tm-int-vs0)))
(display "  TM compiled target:\n")
(pretty-print compiled-tm-target)

(check-equal? (fc-run compiled-tm-target '((1 1 1 0 1 0 1))) '(1 1 0 1))

;; ------------- for FC
(display "\nII Futamura for FC\n\n")
(flush-output)

(define mix-fc-int-vs0
  `((program . ,fc-int)
    (division . ,fc-int-division)
    (blocks-in-pending . '())
    (pp . '())
    (command . '())))

(define fc-compiler (fc-run mix `(,mix ,mix-division ,mix-fc-int-vs0)))
(display "  FC -> FC compiler:\n")
; (pretty-print fc-compiler)

(define compiled-fc-target (fc-run fc-compiler `(,fc-int-vs0)))
(display "  FC compiled target:\n")
(pretty-print compiled-fc-target)

(check-equal? (fc-run compiled-fc-target '((y (x y z) (1 2 3)))) '2)

;; =====================================================================================

;; III Futamura

(display "\n ==  III FUTAMURA == \n")

;; [| mix |] [mix, mix] =: cgen
;; int [cgen, int] -> comp
;; int [comp, source] -> target
;; int [target, input] -> output

(define mix-mix-vs0
  `((program . ,mix)
    (division . ,mix-division)
    (blocks-in-pending . '())
    (pp . '())
    (command . '())))

(define cgen (fc-run mix `(,mix ,mix-division ,mix-mix-vs0)))
(display "cgen:\n\n")
; (pretty-print cgen)

;; ------------- for TM
(display "\nIII Futamura for TM\n\n")
(flush-output)

(define generated-tm-compiler (fc-run cgen `(,mix-tm-int-vs0)))
(display "  TM -> FC generated compiler:\n")
; (pretty-print generated-tm-compiler)

(define generated-compiled-tm-target (fc-run generated-tm-compiler `(,tm-int-vs0)))
(display "  TM generated-compiled target:\n")
(pretty-print generated-compiled-tm-target)

(check-equal? (fc-run generated-compiled-tm-target '((1 1 1 0 1 0 1))) '(1 1 0 1))

;; ------------- for FC
(display "\nIII Futamura for FC\n\n")
(flush-output)

(define generated-fc-compiler (fc-run cgen `(,mix-fc-int-vs0)))
(display "  FC -> FC generated compiler:\n")
; (pretty-print generated-fc-compiler)

(define generated-compiled-fc-target (fc-run generated-fc-compiler `(,fc-int-vs0)))
(display "  FC generated-compiled target:\n")
(pretty-print generated-compiled-fc-target)

(check-equal? (fc-run generated-compiled-fc-target '((y (x y z) (1 2 3)))) '2)
