#lang oa/normal/fancy

;;;; Just enough to implement lists in the fancy language
;;;

(define true (λ (x y) x))
(define false (λ (x y) y))

(define cons (λ (h t s) (s h t)))
(define car (λ l (l true)))
(define cdr (λ l (l false)))
(define nil (λ l true))
(define null? (λ (l) (l (λ (h t) false))))

(test-case
 "conses"
 (check-equiv? (null? nil) true)
 (check-equiv? (car (cons 1 2)) 1)
 (check-equiv? (cdr (cons 1 2)) 2))

;;; Numbers as conses
;;;

(define zero (λ (x) true))
(define zero? null?)
(define pred cdr)
(define succ (λ (l) (cons zero l)))

(test-case
 "numbers/1"
 (check-equiv? (pred (succ zero)) zero)
 (check-equiv? (zero? zero) true)
 (check-not-equiv? (zero? (succ zero)) true))
