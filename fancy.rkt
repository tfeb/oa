#lang racket

;;;; oa/fancy is oa/applicative/fancy
;;;

(module reader syntax/module-reader
  oa/fancy)

(require "applicative/fancy.rkt")
(provide (all-from-out "applicative/fancy.rkt"))