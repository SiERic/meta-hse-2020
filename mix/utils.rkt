#lang racket

(provide lookup)
(provide init-residual)
(provide name-block)
(provide initial-code)
(provide first-command)
(provide rest)
(provide is-static-var?)
(provide substitute)
(provide extend-block)
(provide is-static-expr?)
(provide reduce-expr)
(provide eval-expr)
(provide extend-program)
(provide rename)
(provide car-or-space)
(provide cdr-or-empty)

(define space 2)

(define (car-or-space tape)
  (if (empty? tape) space (car tape)))

(define (cdr-or-empty tape)
  (if (empty? tape) '() (cdr tape)))

(define (lookup pp program)
  (cond 
    [(empty? program) '()]
    [(equal? (caar program) pp) (car program)]
    [else (lookup pp (cdr program))]))

(define (init-residual program division)
  (define read-data (cdar program))
  (list (cons 'read (filter (lambda (x) (not (is-static-var? x division))) read-data))))

(define (name-block pp vs)
  (cons pp (map (lambda (x) (cdr x)) vs))) 

(define (initial-code pp vs)
  (println pp)
  (list (name-block pp vs)))

(define (first-command bb)
  (if (empty? (cdr bb)) 
      '() 
      (cadr bb)))

(define (rest bb)
  (if (empty? (cdr bb)) 
      (list (car bb)) 
      (cons (car bb) (cddr bb))))

(define (is-static-var? var division)
  (if (member var (car division)) #t #f))

(define (is-dynamic-var? var division)
  (if (member var (cadr division)) #t #f))

(define (substitute vs var value)
  (cond
    [(empty? vs) vs]
    [(equal? var (caar vs)) (cons `(,var . ,value) (cdr vs))]
    [else (cons (car vs) (substitute (cdr vs) var value))]))

(define (extend-block code command)
  (append code (list command)))

(define (is-static-expr? expr division)
  (cond
    [(symbol? expr) (not (is-dynamic-var? expr division))]
    [(and (list? expr) (equal? (car expr) 'quote)) #t]
    [(list? expr) (andmap (lambda (e) (is-static-expr? e division)) expr)]
    [else #t]))

(define (reduce-expr expr vs division)
  (if (list? expr)
      (if (equal? (car expr) 'quote)
          expr
          (map (lambda (x) (reduce-expr x vs division)) expr))
      (if (is-static-var? expr division) `',(eval-expr expr vs) expr)))


(define (eval-expr expr vs)
  (define namespace (make-base-namespace))
  (parameterize ([current-namespace namespace]) (eval '(require racket)))
  (parameterize ([current-namespace namespace]) (eval '(require "utils.rkt")))
  (define (set-value var value)
    (parameterize ([current-namespace namespace]) (namespace-set-variable-value! var value)))
  (map (lambda (x) (set-value (car x) (cdr x))) vs)
  (parameterize ([current-namespace namespace]) (eval expr)))

(define (extend-program program bb)
  (append program (list bb)))

(define (rename program)
  (define bb-counter (make-hash))
  (define bb-new-name (make-hash))
  (define (count-bb bb)
    (define number (+ 1 (dict-ref bb-counter (caar bb) -1)))
    (dict-set! bb-counter (caar bb) number)
    (dict-set! bb-new-name (car bb) (string->symbol (string-append (~a (caar bb)) (~a number)))))
  (define (rename-command command)
    (match command
      [`(goto ,label) `(goto ,(dict-ref bb-new-name label))]
      [`(if ,expr ,then-label ,else-label) `(if ,expr ,(dict-ref bb-new-name then-label) ,(dict-ref bb-new-name else-label))]
      [else command]))
  (define (rename-bb bb)
    (cons (dict-ref bb-new-name (car bb)) (map (lambda (c) (rename-command c)) (cdr bb))))
  (for-each (lambda (bb) (count-bb bb)) 
       (cdr program))
  (cons (car program) (map (lambda (bb) (rename-bb bb))
       (cdr program))))
