#lang racket

;;;; Tracing for fancy versions of oa
;;;

(provide
 (contract-out
  (fancy-trace (->* (string?) ()
                      #:rest any/c
                      void?))
  (fancy-tracing (case->
                  (-> any/c)
                  (-> any/c any)))))

(define (rewrite-fancy-app fa)
  (match fa
    [(list* 'fancy:app proc more)
     `(,(rewrite-fancy-app proc)
       ,@(rewrite-fancy-app more))]
    [(list any ...)
     (map rewrite-fancy-app any)]
    [anything anything]))

(define fancy-tracing (make-parameter #f))

(define (fancy-trace fmt . args)
  (when (fancy-tracing)
    (apply eprintf fmt
           (map (λ (s)
                  (if (syntax? s)
                      (rewrite-fancy-app (syntax->datum s))
                      s))
                args))))
