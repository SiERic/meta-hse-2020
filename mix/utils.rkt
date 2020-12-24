#lang racket

(provide next-label-after)
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
(provide get-blocks-in-pending)
(provide clear-vs)
(provide create-ns)
(provide eval-assign-ns)
(provide eval-expr-ns)

(define (create-ns data)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns]) (eval '(require racket)))
  (parameterize ([current-namespace ns]) (eval '(require "utils.rkt")))
  (map (lambda (x) (eval-assign-ns (car x) (cdr x) ns)) data)
  ns)

(define (eval-assign-ns var value ns)
  (parameterize ([current-namespace ns]) (namespace-set-variable-value! var value))
  ns)

(define (eval-expr-ns expr ns)
  (parameterize ([current-namespace ns]) (eval expr)))

(define (get-blocks-in-pending program division)
  (define (get-dynamic-jump-labels bb)
    (match bb
      [(list assignments ... jump)
        (match jump
          [`(if ,expr ,then-label ,else-label) (if (is-static-expr? expr division) '() `(,then-label ,else-label))]
          [else '()])]))
    (cons (caadr program) (remove-duplicates (flatten (map get-dynamic-jump-labels (cdr program))))))

(define (next-label-after pp blocks)
  (cond 
    [(and (equal? (car blocks) pp) (> (length blocks) 1)) (cadr blocks)]
    [(<= (length blocks) 1) '()]
    [else (next-label-after pp (cdr blocks))]))

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
  (flush-output)
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

(define (substitute vs var value)
  (cond
    [(empty? vs) `((,var . ,value))]
    [(equal? var (caar vs)) (cons `(,var . ,value) (cdr vs))]
    [else (cons (car vs) (substitute (cdr vs) var value))]))

(define (clear-vs vs vs0)
  (take vs (length vs0)))

(define (extend-block code command)
  (append code (list command)))

(define (is-static-expr? expr division)
  (cond
    [(symbol? expr) (is-static-var? expr division)] ; 'x
    [(and (list? expr) (empty? expr)) #t]
    [(and (list? expr) (equal? (car expr) 'quote)) #t] ; '(x y z)
    [(list? expr) (andmap (lambda (x) (is-static-expr? x division)) (cdr expr))] ; (f args)
    [else #t])) ; 1

(define (reduce-expr expr vs division)
  (if (is-static-expr? expr division)
    `',(eval-expr expr vs)
    (cond
      [(not (list? expr)) expr]
      [(empty? expr) expr]
      [(equal? (car expr) 'quote) expr]
      [(equal? (car expr) 'lambda) expr]
      [else (cons (car expr) (map (lambda (x) (reduce-expr x vs division)) (cdr expr)))])))

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
