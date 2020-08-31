#lang racket

;;;; oa/pure is oa/applicative/pure
;;;

(module reader syntax/module-reader
  oa/pure)

(require "applicative/pure.rkt")
(provide (all-from-out "applicative/pure.rkt"))