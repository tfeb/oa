#lang racket

(require (only-in rackunit
                  test-case
                  define-binary-check))

(provide test-case check-equiv? check-not-equiv?)

(define-binary-check (check-equiv? a b)
  (eqv? (force a) (force b)))

(define-binary-check (check-not-equiv? a b)
  (not (eqv? (force a) (force b))))
