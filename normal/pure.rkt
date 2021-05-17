#lang racket

;;;; One-arg
;;;

;;; Pure version, normal order: lambda does not have parens for
;;; its single argument, and function application works only with one
;;; argument
;;;

(module reader syntax/module-reader
  oa/normal/pure)

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
          (pure:app #%app)
          (pure:lambda lambda)
          (pure:lambda λ)
          (pure:define define)))

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

(define-syntax (pure:app stx)
  ;; A version of #%app which allows only one argument
  (syntax-case stx ()
    [(_ procedure argument)
     #'((release* procedure) (hold argument))]
    [(_ procedure argument ...)
     (parameterize ([current-syntax-context #'procedure])
       (wrong-syntax #'(procedure argument ...) "need just one argument"))]
    [(_ procedure)
     (parameterize ([current-syntax-context #'procedure])
       (wrong-syntax #'(procedure) "need one argument"))]
    [else
     (parameterize ([current-syntax-context #f])
       (wrong-syntax #'() "not a function application"))]))

(define-syntax (pure:lambda stx)
  ;; single-argument, single-form lambda
  (parameterize ([current-syntax-context stx])
    (syntax-case stx ()
      [(_ (argument ...+) form)
       (wrong-syntax stx "λ doesn't have a list of args")]
      [(_ () form)
       (wrong-syntax stx "don't even try a zero-length list of args for λ")]
      [(_ argument form)
       (identifier? #'argument)
       #'(λ (argument) (hold form))]
      [(_ _ form ...+)
       (wrong-syntax stx "more than one form in λ body")]
      [else
       (wrong-syntax stx "what even is this?")])))

(define-syntax (pure:define stx)
  ;; trivial define
  (parameterize ([current-syntax-context stx])
    (syntax-case stx ()
      [(_ id form)
       (identifier? #'id)
       #'(define id (stash-for-printing 'id form))]
      [(_ (id ...) form ...)
       (wrong-syntax #'(id ...) "fancy defines don't work")]
      [else
       (wrong-syntax stx "what on earth is this?")])))

(print-with-stashes #t)