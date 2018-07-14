#lang racket

;;;; Hacks for the printer for oa
;;;
;;; This lets you stash things for printing in such a way that when printing
;;; something it instead prints its name.  It also turns off printing of quotes
;;; for things.
;;;
;;; This is modifying global state, which is horrid in many ways
;;;

(provide stash-for-printing
         print-with-stashes)

(define stashed (make-weak-hasheqv))

(define print-with-stashes (make-parameter #f))

(define (stash-for-printing id v)
  (hash-set! stashed v id)
  v)

(current-print
 (let ([p (current-print)])
   (λ (v)
     (if (print-with-stashes)
         (parameterize ([print-as-expression #f])
           (p (if (hash-has-key? stashed v)
                  (hash-ref stashed v)
                  v)))
         (p v)))))
