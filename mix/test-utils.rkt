#lang racket
(require "utils.rkt")
(require "mixer.rkt")
(require rackunit)
(require plai-typed/s-exp-match)

(define program-example
  '((read name namelist valuelist)
    (search (if (equal? name (car namelist)) found cont))
    (cont (:= valuelist (cdr valuelist))
          (:= namelist  (cdr namelist))
          (goto search))
    (found (return (car valuelist)))))

(define pp-example 
  'search)

; static / dynamic
(define division-example 
  '((name namelist) 
    (valuelist)))

(define vs-example 
  '((name . y)
    (namelist . (x y z))))

(define bb-example 
  (lookup 'cont program-example))

(check-equal? (lookup 'found program-example) '(found (return (car valuelist))))
(check-equal? (lookup 'kek program-example) '())

(check-equal? (init-residual program-example division-example) '((read valuelist)))

(check-equal? (name-block pp-example vs-example) '(search y (x y z)))

(check-equal? (initial-code pp-example vs-example) '((search y (x y z))))

(check-equal? (first-command bb-example) '(:= valuelist (cdr valuelist)))
(check-equal? (first-command '(kek)) '())

(check-equal? (rest bb-example) '(cont (:= namelist (cdr namelist)) (goto search)))
(check-equal? (rest '(kek (lol))) '(kek))
(check-equal? (rest '(kek)) '(kek))

(check-equal? (is-static-var? 'name division-example) #t)
(check-equal? (is-static-var? 'valuelist division-example) #f)

(check-equal? (substitute vs-example 'name 'lol) '((name . lol) (namelist . (x y z))))
(check-equal? (substitute vs-example 'namelist '(a b c)) '((name . y) (namelist . (a b c))))
(check-equal? (substitute vs-example 'kek 'lol) '((name . y) (namelist . (x y z)) (kek . lol)))

(check-equal? (clear-vs (substitute vs-example 'kek 'lol) vs-example) vs-example)

(check-equal? (extend-block '(kek (:= lol heh)) '(:= heh lol)) '(kek (:= lol heh) (:= heh lol)))

(check-equal? (is-static-expr? '(name) division-example) #t)
(check-equal? (is-static-expr? '(:= name (car namelist)) division-example) #t)
(check-equal? (is-static-expr? '(car valuelist) division-example) #f)
(check-equal? (is-static-expr? '(cons 1 namelist) division-example) #t)
(check-equal? (is-static-expr? '() division-example) #t)
(check-equal? (is-static-expr? ''(x y z) division-example) #t)

(check-equal? (eval-expr '(car namelist) vs-example) 'x)
(check-equal? (eval-expr '(cons name namelist) vs-example) '(y x y z))
(check-equal? (eval-expr ''(x y z) vs-example) '(x y z))
(check-equal? (eval-expr ''(name namelist) vs-example) '(name namelist))
(check-equal? (eval-expr `(lookup 'found (quote ,program-example)) vs-example) (lookup 'found program-example))

(check-equal? (extend-program '((kek (goto lol)) (lol (goto heh))) '(heh (goto kek)))
                              '((kek (goto lol)) (lol (goto heh)) (heh (goto kek))))

(println (reduce-expr '(cons name valuelist) vs-example division-example))
(println (reduce-expr '(cdr namelist) vs-example division-example))
(println (reduce-expr 'namelist vs-example division-example))

(println (reduce-expr ''() vs-example division-example))
(println (reduce-expr ''(lambda (x) (* x x)) vs-example division-example))
(println (reduce-expr '(map (lambda (y) (* y y)) namelist) vs-example division-example))
(println (reduce-expr '(set) vs-example division-example))

(define new-program
  '((read kek)
    ((lol 1 2 3) (:= heh 95) (goto (lol 1 4 5)))
    ((lol 1 4 5) (:= heh 96) (goto (end 118 23 45)))
    ((end 118 23 45) (return kek))))
