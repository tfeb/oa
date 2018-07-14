#lang racket

;;;; One-arg
;;;
;;; Fancy / lispy version: lambda either be (lambda arg val) or (lambda (arg ...) val)
;;; which turns into the nested thing & function application uncurries
;;;

(module reader syntax/module-reader
  oa/fancy)

;;; Things from racket we need
;;;
(provide
 #%module-begin
 #%top-interaction
 #%top
 #%datum)

;;; Restricted versions of some things
(provide (rename-out
          (fancy:app #%app)
          (fancy:lambda lambda)
          (fancy:lambda λ)
          (fancy:define define)))

(require (for-syntax (only-in racket/syntax
                              current-syntax-context
                              wrong-syntax))
         (only-in "private/printer-hax.rkt"
                  stash-for-printing
                  print-with-stashes))

(define-syntax (fancy:app stx)
  ;; A version of #%app which allows only one argument
  (syntax-case stx ()
    [(_ procedure argument)
     #'(procedure argument)]
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
      [(_ (argument) form)
       #'(λ (argument) form)]
      [(_ (argument more ...) form)
       #'(λ (argument) (fancy:lambda (more ...) form))]
      [(_ () form)
       (wrong-syntax stx "zero-argument λ")]
      [(_ argument form)
       (identifier? #'argument)
       #'(λ (argument) form)]
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
       #'(define id (stash-for-printing 'id form))]
      [(_ (id ...) form ...)
       (wrong-syntax #'(id ...) "fancy defines don't work")]
      [else
       (wrong-syntax stx "what on earth is this?")])))

(print-with-stashes #t)