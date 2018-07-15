#lang racket

;;;; oa/pure is oa/applicative/pure
;;;

(module reader syntax/module-reader
  oa/pure)

(require "applicative/fancy.rkt")
(provide (all-from-out "applicative/fancy.rkt"))