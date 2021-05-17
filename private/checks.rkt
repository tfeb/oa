#lang racket

(require (only-in rackunit
                  test-case
                  define-binary-check)
         (only-in "hold.rkt"
                  release*))

(provide test-case check-equiv? check-not-equiv?)

(define-binary-check (check-equiv? a b)
  (eqv? (release* a) (release* b)))

(define-binary-check (check-not-equiv? a b)
  (not (eqv? (release* a) (release* b))))
