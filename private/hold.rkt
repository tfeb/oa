#lang racket

;;;; A simple-minded version of delay & force
;;; This is so they can be instrumented: hold is delay, release is force
;;;

(provide hold
         (contract-out
          (release (-> any/c any/c))
          (release* (-> any/c any/c))
          (hold-debugging (case->
                            (-> any/c)
                            (-> any/c any)))))

(define hold-debugging (make-parameter #f))

(define (dbp fmt . args)
  (when (hold-debugging)
    (apply eprintf fmt args)))

(struct held
  ((thing #:mutable)
   source
   (released? #:mutable #:auto))
  #:methods gen:custom-write
  ((define (write-proc it port mode)
     ;; This is horrid manual unreadable-but-useful printing: there probably
     ;; is a better approach to this.
     (when mode (write-string "#<" port))
     (fprintf port "~S" (held-source it))
     (when mode (write-string ">" port)))))

(require racket/undefined)

(define-syntax-rule (hold form)
  ;; hold a form for later releasing
  (begin
    (dbp "[holding ~S]~%" 'form)
    (held (thunk form) 'form)))

(define (release holder)
  ;; release something if it is held
  (cond [(not (held? holder))
         (dbp "[unheld ~S]~%" holder)
         holder]
        [(held-released? holder)
         (dbp "[already ~S]~%" (held-source holder))
         (held-thing holder)]
        [else
         (dbp "[releasing ~S]~%" (held-source holder))
         (let ([v ((held-thing holder))])
           (set-held-thing! holder v)
           (set-held-released?! holder #t)
           v)]))

(define (release* maybe-holder)
  ;; Keep releasing until maybe-holder is not held
  (if (held? maybe-holder)
      (release* (release maybe-holder))
      maybe-holder))
