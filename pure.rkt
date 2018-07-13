#lang racket

;;;; One-arg
;;;
;;; Pure version: lambda does not have parens for its argument,
;;; and function application uncurries: (f a b) is ((f a) b)
;;;

(module reader syntax/module-reader
  oa/pure)

;;; Things from racket we need
;;;
(provide
 #%module-begin
 #%top-interaction
 #%top
 #%datum)

;;; Restricted versions of some things
(provide (rename-out
          (pure:app #%app)
          (pure:lambda lambda)
          (pure:lambda λ)
          (pure:define define)))

(require (for-syntax (only-in racket/syntax
                              current-syntax-context
                              wrong-syntax)))
(define-syntax (pure:app stx)
  ;; A version of #%app which allows only one argument
  (syntax-case stx ()
    [(_ procedure argument)
     #'(procedure argument)]
    [(_ procedure argument more ...)
     #'(pure:app (procedure argument) more ...)]
    [(_ procedure)
     (parameterize ([current-syntax-context #'procedure])
       (wrong-syntax #'(procedure) "need one argument"))]
    [else
     (parameterize ([current-syntax-context #f])
       (wrong-syntax #'() "not a function application"))]))

(define-syntax-rule (pure:lambda arg form)
  ;; single-argument, single-form lambda
  (lambda (arg) form))

(define-syntax (pure:define stx)
  ;; trivial define
  (parameterize ([current-syntax-context stx])
    (syntax-case stx ()
      [(_ (id ...) form ...)
       (wrong-syntax #'(id ...) "fancy defines don't work")]
      [(_ id form)
       #'(define id form)])))
