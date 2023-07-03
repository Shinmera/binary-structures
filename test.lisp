(in-package #:org.shirakumo.binary-structures)

(define-io-structure test2
  (a uint16)
  (b (string uint8))
  (c (string uint8)))
