#lang racket

;;;; One-arg
;;;

(module reader syntax/module-reader
  oa)

;;; Things from racket we need
;;;
(provide
 #%module-begin
 #%top-interaction
 #%top
 #%datum)

;;; Restricted versions of some things
(provide (rename-out
          (oa:app #%app)
          (oa:lambda lambda)
          (oa:lambda λ)
          (oa:define define)))

(require (for-syntax (only-in racket/syntax
                              current-syntax-context
                              wrong-syntax)))
(define-syntax (oa:app stx)
  ;; A version of #%app which allows only one argument
  (syntax-case stx ()
    [(_ procedure argument)
     #'(procedure argument)]
    [(_ proc)
     (parameterize ([current-syntax-context #'proc])
       (wrong-syntax #'(proc) "need one argument"))]
    [(_ proc arg ...)
     (parameterize ([current-syntax-context #'proc])
       (wrong-syntax #'(proc arg ...) "need just one argument"))]
    [else1
     (parameterize ([current-syntax-context #f])
       (wrong-syntax #'() "not a function application"))]))

(define-syntax-rule (oa:lambda (arg) form)
  ;; single-argument, single-form lambda
  (lambda (arg) form))

(define-syntax (oa:define stx)
  ;; trivial define
  (parameterize ([current-syntax-context stx])
    (syntax-case stx ()
      [(_ (id ...) form ...)
       (wrong-syntax #'(id ...) "fancy defines don't work")]
      [(_ id form)
       #'(define id form)])))
