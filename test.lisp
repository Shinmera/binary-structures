(in-package #:org.shirakumo.binary-structures)

(define-io-flags test-flags uint32
  ;; a "no-flags-set" flag (not actually valid for this definition
  ;; since :flag56 requires at least 1 bit set)
  (#x0000 :nothing)
  ;; normal single-bit flags
  (#x0001 :flag1)
  (#x0002 :flag2)
  ;; flags can overlap, for example if we want :read,:write,:read-write
  (#x0003 :flag12)
  ;; If we are implementing a spec that gives a mask and a set of
  ;; masked values for named combinations of those bits, we can use
  ;; those directly.
  (#x000c :flag34
          (#x0000 :low-none)
          (#x0004 :low-a)
          (#x0008 :low-b)
          (#x000c :low-c))
  ;; If we are given a bit range and masked values, we can use
  ;; MASK-FIELD with a byte-spec instead
  ((mask-field (byte 2 4)) :flag56
   (#x10 :mid-a)
   (#x20 :mid-b)
   (#x30 :mid-c))
  ;; If given a bit range and values for the extracted value, we can
  ;; use LDB
  ((ldb (byte 2 6)) :flag78
   (#x0 :high-none)
   (#x1 :high-a)
   (#x3 :high-c))
  ;; alternate syntax for the first style. Either masking syntax
  ;; allows disjoint sets of bits.
  ((logand #x10700) :flag9abg
   (#x00700 :split-a)
   (#x10100 :split-b)
   (#x10200 :split-c))
  ;; and some flags in the middle
  (#x1000 :flagc)
  (#x2000 :flagd)
  (#x4000 :flage))

;; define-io-flags defines a function to convert a flag value to a
;; list of keys, mostly for debugging/printing
(assert (equalp (test-flags-as-keys #b10110000100101110)
                '(:FLAGE :FLAGD :SPLIT-B :HIGH-NONE :MID-B :LOW-C :FLAG2)))

(define-io-structure test2
  (a uint16)
  (b (string uint8))
  (c (string uint8))
  (flags test-flags))

(flet ((octet-vector (&rest r)
         (coerce r '(simple-array (unsigned-byte 8) 1))))
  (let* ((a (octet-vector #x00
                          #x12 #x34
                          3 102 111 111
                          3 98 97 114
                          #x79 #x37 #x00 #x00 ))
         (test (read-test2 a :start 1)))
    (assert (= (test2-a test) #x3412))
    (assert (string= (test2-b test) "foo"))
    (assert (string= (test2-c test) "bar"))
    ;; flags slot is available directly as a number
    (assert (= (test2-flags test) #x3779))
    ;; can be tested against individual flags
    (assert (test2-flags-p test :flag1))
    (assert (not (test2-flags-p test :flag2)))
    (assert (not (test2-flags-p test :flag12)))
    (assert (test2-flags-p test :flagc))
    (assert (test2-flags-p test :flagd))
    (assert (not (test2-flags-p test :flage)))
    ;; can test value of a group flag
    (assert (eql (test2-flags-p test :flag34) :low-b))
    (assert (eql (test2-flags-p test :flag56) :mid-c))
    (assert (eql (test2-flags-p test :flag78) :high-a))
    ;; groups can be extracted by name (useful when using SLOT within
    ;; a definition
    (assert (eql (test2-flags-flag9abg test) :split-a))
    ;; we can set flags as well
    (setf (test2-flags-p test :flag34) :low-none) ;; clear both bits
    (setf (test2-flags-p test :flagc) nil) ;; clear bit
    (setf (test2-flags-p test :flage) t) ;; set bit
    (assert (eql (test2-flags test) #x6771))
    (setf (test2-flags-p test :flag2) t)
    (assert (test2-flags-p test :flag2))
    (assert (test2-flags-p test :flag12))
    (setf (test2-flags-p test :flag12) nil) ;; clears flag1 and flag2 at once
    (assert (not (test2-flags-p test :flag1)))
    (assert (not (test2-flags-p test :flag2)))
    (assert (not (test2-flags-p test :flag12)))
    (setf (test2-flags-p test :flag12) t) ;; or set flag1 and flag2 at once
    (assert (test2-flags-p test :flag1))
    (assert (test2-flags-p test :flag2))
    (assert (test2-flags-p test :flag12))
    (let ((b (make-array (length a) :element-type '(unsigned-byte 8))))
      (write-test2 test b)
      (assert (equalp b #(18 52 3 102 111 111 3 98 97 114 115 103 0 0 0))))))
