#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-structure bitmapcoreheader
  (width uint16)
  (height uint16)
  (planes uint16)
  (bits/pixel uint16))

(define-io-alias bitmapcompression
  (case uint32
    (0 :rgb)
    (1 :rle8)
    (2 :rle4)
    (3 :bitfields)
    (4 :jpeg)
    (5 :png)
    (6 :alpha-bitfields)
    (11 :cmyk)
    (12 :cmyk-rle8)
    (13 :cmyk-rle4)))

(define-io-structure bitmapinfoheader
  (width sint32)
  (height sint32)
  (planes uint16)
  (bits/pixel uint16)
  (compression bitmapcompression)
  (image-size uint32)
  (horizontal-resolution sint32)
  (vertical-resolution sint32)
  (palette-size uint32)
  (important-color-count uint32))

(define-io-structure bitmapv2infoheader
  (:include bitmapinfoheader)
  (red-mask uint32)
  (green-mask uint32)
  (blue-mask uint32))

(define-io-structure bitmapv3infoheader
  (:include bitmapv2infoheader)
  (alpha-mask uint32))

(define-io-structure xyz
  (x :uint32) ; FXPT2DOT30, 2 bits int, 30 bits fractional.
  (y :uint32)
  (z :uint32))

(define-io-structure bitmapv4infoheader
  (:include bitmapv3infoheader)
  (cs-type uint32)
  (red-endpoint xyz)
  (green-endpoint xyz)
  (blue-endpoint xyz)
  (red-gamma uint32)
  (green-gamma uint32)
  (blue-gamma uint32))

(define-io-structure bitmapv5infoheader
  (:include bitmapv4infoheader)
  (intent uint32)
  (profile-data uint32)
  (profile-size uint32)
  uint32)

(define-io-alias halftoning-algorithm
  (case uint16
    (0 NIL)
    (1 :error-diffusion)
    (2 :panda)
    (3 :super-circle)))

(define-io-structure os22xbitmapheader
  (:include bitmapinfoheader)
  (resolution-unit uint16)
  uint16
  (origin uint16)
  (halftoning halftoning-algorithm)
  (halftoning-parameter-1 sint32)
  (halftoning-parameter-2 sint32)
  (color-encoding uint32)
  (identifier uint32))

(define-io-structure os22xbitmapheader/short
  (width sint32)
  (height sint32)
  (planes uint16)
  (bits/pixel uint16)
  (compression bitmapcompression))

(define-io-structure bmp
  "BM"
  (size uint32)
  uint16 uint16
  (bitmap-offset uint32)
  (header (case uint32
            (12 bitmapcoreheader)
            (16 os22xbitmapheader/short)
            (40 bitmapinfoheader)
            (52 bitmapv2infoheader)
            (56 bitmapv3infoheader)
            (64 os22xbitmapheader)
            (108 bitmapv4header)
            (124 bitmapv5header)))
  (bit-masks (typecase (slot header)
               (bitmapinfoheader
                (case (slot header compression)
                  (:bitfields)
                  (:alpha-bitfields)))
               (T NIL)))
  (color-table (typecase (slot header)
                 (bitmapcoreheader
                  (vector uint8 (* 3 (expt 2 (slot header bits/pixel)))))
                 (bitmapinfoheader
                  (vector uint8 (* 4 (slot header palette-size))))
                 (T NIL)))
  (pixels (vector uint8 (slot header image-size))
          :offset (slot bitmap-offset)))

;;;; TODO:
;; slot descending
;; dynamic offset
;; typecase
;; math expressions (* / - + ash expt)

(define-io-functions bmp)

(define-io-structure ico-entry
  (width uint8)
  (height uint8)
  (palette-size uint8)
  uint8
  (property-1 uint16)
  (property-2 uint16)
  (octet-size uint32)
  (offset uint32))

(define-io-structure ico
  uint16
  (type (case uint16
          (1 :ico)
          (2 :cur)))
  (count uint16)
  (entries (vector ico-entry (slot-value count)))
  )
