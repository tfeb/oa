#lang racket/base

;;;; oa/applicative is oa/applicative/pure
;;;

(module reader syntax/module-reader
  oa/applicative)

(require "applicative/pure.rkt")
(provide (all-from-out "applicative/pure.rkt"))
