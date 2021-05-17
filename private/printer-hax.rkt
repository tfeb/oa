#lang racket

;;;; Hacks for the printer for oa
;;;
;;; This lets you stash things for printing in such a way that when printing
;;; something it instead prints its name.  Also disable printing quotes.
;;;
;;; This is modifying global state, which is horrid in many ways
;;;

(provide (contract-out
          (stash-for-printing
           (->* (any/c symbol?) (any/c) any/c))
          (print-with-stashes
           (case->
            (-> boolean?)
            (-> boolean? any)))))

(require (only-in "hold.rkt"
                  release*))

(define (rewrite-fancy-lambda flf)
  ;; Rewrite fancy lambda source forms away, reducing everything to its
  ;; pure equivalent
  (match flf
    [(list (or 'λ 'fancy:lambda) (list argument) form)
     `(λ ,argument ,(rewrite-fancy-lambda form))]
    [(list (or 'λ 'fancy:lambda) (list argument more ...) form)
     `(λ ,argument ,(rewrite-fancy-lambda `(fancy:lambda ,more ,form)))]
    [(list (or 'λ 'fancy:lambda) argument form)
     `(λ ,argument ,(rewrite-fancy-lambda form))]
    [anything anything]))

(define print-with-stashes (make-parameter #f))

;;; stashed maps from values to a cons of a set of names and the source
;;; where the source may be void.
(define stashed (make-weak-hasheqv))

(define (stash-for-printing value name (source (void)))
  ;; Possibly tash a value for printing, returning it.
  (when (print-with-stashes)
    (set-add! (car (hash-ref! stashed value (thunk
                                             (cons
                                              (mutable-seteqv)
                                              (rewrite-fancy-lambda source)))))
              name))
  value)

(current-print
 (let ([p (current-print)])
   (λ (v)
     (let ([vv (release* v)])
       (if (print-with-stashes)
           (parameterize ([print-as-expression #f]) ;stop quotes
             (let ([found (hash-ref stashed vv #f)])
               (cond
                 [found
                  (parameterize ([print-pair-curly-braces #t])
                    ;; print the set of names in order, looking like a set
                    (display (sort (set->list (car found))
                                   symbol<?)
                             (current-output-port)))
                  (when (not (void? (cdr found)))
                    ;; There's a source, so print it
                    (fprintf (current-output-port)
                             ": ~A" (cdr found)))]
                 [(procedure? vv)
                  ;; Suppress useless information for procedures
                  (display "#<λ>" (current-output-port))]
                 [else
                  ;; punt
                  (p vv)])))
           (p vv))))))