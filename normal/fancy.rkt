#lang racket

;;;; One-arg
;;;
;;; Fancy / lispy version, normal order
;;; - lambda can be: (lambda arg form), (lambda (arg) form) which
;;;   turns into (lambda arg form) or (lambda (arg more ...) form)
;;;   which turns into ((lambda arg (lambda (more ...) form)))
;;; - function application can take more than one argument and uncurries:
;;;   (f a b ...) is ((f a) b ...)
;;;
;;; This is much more flaky than the pure one.
;;;

(module reader syntax/module-reader
  oa/normal/fancy)

;;; Things from racket we need
;;;
(provide
 #%module-begin
 #%top-interaction
 #%top
 #%datum)

;;; Checks (a subset of rackunit)
;;;
(require "../private/checks.rkt")
(provide (all-from-out "../private/checks.rkt"))

;;; Restricted versions of some things
(provide (rename-out
          (fancy:app #%app)
          (fancy:lambda lambda)
          (fancy:lambda λ)
          (fancy:define define)))

(require (for-syntax (only-in racket/syntax
                              current-syntax-context
                              wrong-syntax))
         (only-in "../private/printer-hax.rkt"
                  stash-for-printing
                  print-with-stashes)
         (only-in "../private/hold.rkt"
                  hold release release*
                  hold-debugging))

(hold-debugging (if (getenv "OA_HOLD_DEBUGGING") #t #f))

(print-with-stashes (if (getenv "OA_NO_STASH_PRINTING") #f #t))

(define-syntax (fancy:app stx)
  ;; A version of #%app which allows only one argument
  (syntax-case stx ()
    [(_ procedure argument)
     #'((release* procedure) (hold argument))]
    [(_ procedure argument more ...)
     #'(fancy:app (procedure argument) more ...)]
    [(_ procedure)
     (parameterize ([current-syntax-context #'procedure])
       (wrong-syntax #'(procedure) "need one argument"))]
    [else
     (parameterize ([current-syntax-context #f])
       (wrong-syntax #'() "not a function application"))]))

(define-syntax (fancy:lambda stx)
  ;; A slightly fancy single-form lambda
  (parameterize ([current-syntax-context stx])
    (syntax-case stx ()
      [(_ argument form)
       (identifier? #'argument)
       #'(λ (argument) (hold form))]
      [(_ (argument) form)
       (identifier? #'argument)
       #'(fancy:lambda argument form)]
      [(_ (argument more ...) form)
       (identifier? #'argument)
       #'(fancy:lambda argument (fancy:lambda (more ...) form))]
      [(_ () form)
       (wrong-syntax stx "zero-argument λ")]
      [(_ _ form ...+)
       (wrong-syntax stx "more than one form in λ body")]
      [else
       (wrong-syntax stx "what even is this?")])))

(define-syntax (fancy:define stx)
  ;; trivial define
  (parameterize ([current-syntax-context stx])
    (syntax-case stx ()
      [(_ id form)
       (identifier? #'id)
       #'(define id (stash-for-printing form 'id 'form))]
      [(_ (id ...) form ...)
       (wrong-syntax #'(id ...) "fancy defines don't work")]
      [else
       (wrong-syntax stx "what on earth is this?")])))