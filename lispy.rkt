#lang racket

;;;; One-arg
;;;
;;; Lispy version: lambda has parens around the argument and function
;;; application does not uncurry.
;;;

(module reader syntax/module-reader
  oa/lispy)

;;; Things from racket we need
;;;
(provide
 #%module-begin
 #%top-interaction
 #%top
 #%datum)

;;; Restricted versions of some things
(provide (rename-out
          (lispy:app #%app)
          (lispy:lambda lambda)
          (lispy:lambda λ)
          (lispy:define define)))

(require (for-syntax (only-in racket/syntax
                              current-syntax-context
                              wrong-syntax)))
(define-syntax (lispy:app stx)
  ;; A version of #%app which allows only one argument
  (syntax-case stx ()
    [(_ procedure argument)
     #'(procedure argument)]
    [(_ procedure argument ...)
     (parameterize ([current-syntax-context #'procedure])
       (wrong-syntax #'(procedure argument ...) "need just one argument"))]
    [(_ procedure)
     (parameterize ([current-syntax-context #'procedure])
       (wrong-syntax #'(procedure) "need one argument"))]
    [else
     (parameterize ([current-syntax-context #f])
       (wrong-syntax #'() "not a function application"))]))

(define-syntax-rule (lispy:lambda (arg) form)
  ;; single-argument, single-form lambda
  (lambda (arg) form))

(define-syntax (lispy:define stx)
  ;; trivial define
  (parameterize ([current-syntax-context stx])
    (syntax-case stx ()
      [(_ (id ...) form ...)
       (wrong-syntax #'(id ...) "fancy defines don't work")]
      [(_ id form)
       #'(define id form)])))
