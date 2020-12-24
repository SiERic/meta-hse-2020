#lang racket

(provide fc-run)

;; FlowChart interpreter on Racket

(define (fc-run program data)
  (define data-vars (cdar program))
  (define blocks (cdr program))
  
  (define namespace (make-base-namespace))
  (parameterize ([current-namespace namespace]) (eval '(require racket)))
  (parameterize ([current-namespace namespace]) (eval '(require "utils.rkt")))
  (define (set-value var value)
    (parameterize ([current-namespace namespace]) (namespace-set-variable-value! var value)))
  (define (eval-expr expr)
    (parameterize ([current-namespace namespace]) (eval expr)))
  (define (save-label block)
    (set-value (car block) (cdr block)))
  
  (define (eval-assignment assignment)
    (match assignment
      [`(:= ,var ,expr)
       (set-value var (eval-expr expr))]))
  
  (define (goto label)
    (eval-block (eval-expr label)))
  (define (eval-jump jump)
    (match jump
      [`(goto ,label) (goto label)]
      [`(if ,expr ,then-label ,else-label) 
        (if (eval-expr expr) (goto then-label) (goto else-label))]
      [`(return ,expr) (eval-expr expr)]))
  
  (define (eval-block block)
    (match block
      [(list assignments ... jump)
        (map eval-assignment assignments)
        (eval-jump jump)]))

  (map set-value data-vars data)
  (map save-label blocks)
  (eval-block (cdar blocks)))