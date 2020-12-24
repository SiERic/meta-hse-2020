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

; (define mixed-program (fc-run mix `(,program-example ,division-example ,vs0-example)))
; (check-equal? (fc-run mixed-program '((1 2 3))) '2)

; (pretty-print mixed-program)

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

; (define target (fc-run mix `(,tm-int ,tm-int-division ,tm-int-vs0)))
; (check-equal? (fc-run target '((1 1 1 0 1 0 1))) '(1 1 0 1))

; (pretty-print target)

; (pretty-print target) output:

; '((read right)
;   (init0 
;    (:= left '()) 
;    (:= symbol '0) 
;    (if (equal? symbol (car right)) jump0 loop0))
;   (jump0
;    (:= symbol '1)
;    (:= right (cons symbol (cdr right)))
;    (return (dropf-right right (lambda (x) (eq? x '2)))))
;   (loop0
;    (:= left (cons (car-or-space right) left))
;    (:= right (cdr-or-empty right))
;    (:= symbol '0)
;    (if (equal? symbol (car right)) jump0 loop0)))

;; =====================================================================================

;; II Futamura

;; [| mix |] [mix, int] =: comp
;; int [comp, source] -> target
;; int [target, input] -> output

(define mix-tm-int-vs0
  `((program . ,tm-int)
    (division . ,tm-int-division)
    (blocks-in-pending . '())
    (pp . '())
    (command . '())))

(define mix-division
  '((program division blocks-in-pending pp bb command x expr pp-then pp-else)
    (vs0 pending marked residual ppp vs code)))

(define mix-strange-vs0
  `((program . ,program-example)
    (division . ,division-example)
    (blocks-in-pending . '())
    (pp . '())
    (command . '())))

; (define strange-one (fc-run mix `(,mix ,mix-division ,mix-strange-vs0)))

; (pretty-print strange-one)

; (require "strange.rkt")

; (fc-run strange-one `(,vs0-example))

; doesnt work ((

; (define comp (fc-run mix `(,mix ,mix-division ,mix-tm-int-vs0)))

; (pretty-print comp)

; (fc-run comp `(,tm-int-vs0))
; (define compiled-target (fc-run comp `(,tm-int-vs0)))
; (define compiled-target
  ; '((read right) (init0 (:= left '()) (:= symbol '0) (if (equal? symbol (car right)) jump0 loop0)) (jump0 (:= symbol '1) (:= right (cons symbol (cdr right))) (return (dropf-right right (lambda (x) (eq? x '2))))) (loop0 (:= left (cons (car-or-space right) left)) (:= right (cdr-or-empty right)) (:= symbol '0) (if (equal? symbol (car right)) jump0 loop0)))
  ; )

; (check-equal? (fc-run compiled-target '((1 1 1 0 1 0 1))) '(1 1 0 1))

; (pretty-print compiled-target)

; (require "kek.rkt")

; (fc-run comp `(,tm-int-vs0))

;; =====================================================================================

;; III Futamura

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

; (define cgen (fc-run mix `(,mix ,mix-division ,mix-mix-vs0)))

; (pretty-print cgen)

; (require "cgen.rkt")

; (define generated-tm-compiler (fc-run cgen `(,mix-tm-int-vs0)))

; (pretty-print generated-tm-compiler)

; (require "compiler2.rkt")

; (define compiled-target (fc-run generated-compiler `(,tm-int-vs0)))

; (pretty-print compiled-target)

(define generated-compiled-target
  '((read right)
  (init0 (:= left '()) (:= symbol '0) (if (equal? symbol (car right)) jump0 loop0))
  (jump0
   (:= symbol '1)
   (:= right (cons symbol (cdr right)))
   (return (dropf-right right (lambda (x) (eq? x '2)))))
  (loop0
   (:= left (cons (car-or-space right) left))
   (:= right (cdr-or-empty right))
   (:= symbol '0)
   (if (equal? symbol (car right)) jump0 loop0))))

(check-equal? (fc-run generated-compiled-target '((1 1 1 0 1 0 1))) '(1 1 0 1))
