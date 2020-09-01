#lang racket/base

;;;; oa/normal is oa/normal/pure
;;;

(module reader syntax/module-reader
  oa/normal)

(require "normal/pure.rkt")
(provide (all-from-out "normal/pure.rkt"))
