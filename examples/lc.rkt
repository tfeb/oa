#lang oa/normal/pure

(define cond (λ p (λ a (λ b ((p a) b)))))
(define true (λ x (λ y x)))
(define false (λ x (λ y y)))
(define and (λ a (λ b ((a b) false))))
(define or (λ a (λ b ((a true) b))))
(define not (λ a (((cond a) false) true)))

(test-case
 "conditionals"
 (check-equiv? (((cond true) 1) 2) 1)
 (check-equiv? (((cond false) true) false) false)
 (check-equiv? ((and true) true) true)
 (check-equiv? ((and false) true) false)
 (check-equiv? ((and true) false) false)
 (check-equiv? ((and false) false) false)
 (check-equiv? ((or false) false) false)
 (check-equiv? ((or false) true) true)
 (check-equiv? ((or true) false) true)
 (check-equiv? ((or true) true) true)
 (check-equiv? (not true) false)
 (check-equiv? (not false) true))

;;; Conses
;;;

(define cons (λ h (λ t (λ s ((s h) t)))))
(define car (λ l (l true)))
(define cdr (λ l (l false)))
(define nil (λ l true))
(define null? (λ l (l (λ h (λ t false)))))

(test-case
 "conses"
 (check-equiv? (null? nil) true)
 (check-equiv? (car ((cons 1) 2)) 1)
 (check-equiv? (cdr ((cons 1) 2)) 2))

;;; Numbers as conses
;;;

(define zero (λ x true))
(define zero? null?)
(define pred cdr)
(define succ (λ l ((cons zero) l)))

(test-case
 "numbers/1"
 (check-equiv? (pred (succ zero)) zero)
 (check-equiv? (zero? zero) true)
 (check-not-equiv? (zero? (succ zero)) true))

;;; Everything below here is cheating as functions use themselves free:
;;; it should use Y or U instead.
;;;

(define =
  ;; This won't work in an applicative-order language!
  (λ a (λ b
         ((or ((and (zero? a)) (zero? b)))
          ((and (not ((or (zero? a)) (zero? b))))
           ((= (pred a)) (pred b)))))))

(define +
  (λ a (λ b
         (((cond (zero? b))
           a)
          ((+ (succ a)) (pred b))))))

(test-case
 "numbers/2"
 (check-equiv? ((= (succ (succ (succ zero))))
                (succ (succ (succ zero))))
               true)
 (check-equiv? ((= (pred (succ zero)))
                zero)
               true)
 ;; 1 + 1 = 2
 (check-equiv? ((= ((+ (succ zero)) (succ zero)))
               (succ (succ zero)))
               true))

;;; nth element of a list
(define nth
  (λ n (λ c
         (((cond (zero? n))
           (car c))
          ((nth (pred n)) (cdr c))))))

(test-case
 "nth"
 (check-equiv? ((nth (succ zero))
                ((cons (succ zero))
                 ((cons zero) nil)))
               zero)
 (check-equiv? (pred ((nth zero)
                      ((cons (succ zero))
                       ((cons zero) nil))))
               zero))
