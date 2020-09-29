#lang racket

(require racket/trace)
(provide tm-run)

;; Post Turing Machine interpreter

(define space 2)

(define (space-pad tape)
  (if (empty? tape)
      `(,space)
      tape))

(define (clean-space tape)
  (dropf-right tape (lambda (x) (eq? x space))))

(define (tm-run program input) 
  (define (tm-run-pos program pos lleft rright)
    (define left  (space-pad lleft))
    (define right (space-pad rright))
    (if (eq? pos (length program))
    (clean-space right)
    (match (cdr (list-ref program pos))
      [`{left}          (tm-run-pos program (+ pos 1) (drop left 1)                 (append (take-right left 1) right))]
      [`{right}         (tm-run-pos program (+ pos 1) (append left `(,{car right})) (cdr right))]
      [`{write ,a}      (tm-run-pos program (+ pos 1) left                          (list-set right 0 a))]
      [`{goto ,i}       (tm-run-pos program i         left                          right)]
      [`{if ,a goto ,i} 
                      (if (eq? (car right) a)
                        (tm-run-pos program i         left                          right)
                        (tm-run-pos program (+ pos 1) left                          right))])))
  (tm-run-pos program 0 '() input))
