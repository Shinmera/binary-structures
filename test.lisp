#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-structure archive-metadata-entry
  (mod-time uint64)
  (checksum uint32)
  (mime-type (string uint8))
  (path (string uint16)))

(define-io-functions archive-metadata-entry)

(define-io-structure archive
  (count uint64)
  (metadata-size uint64)
  (entry-offsets (vector uint64 (slot-value count)))
  (metadata (vector archive-metadata-entry (slot-value count)))
  (file-offsets (vector uint64 (slot-value count)))
  (files (vector (vector uint8 uint64) (slot-value count))))

(define-io-functions archive)

(define-io-structure audio
  (samplerate uint32)
  (channels uint8)
  (format 
   (case uint8
     (#x01 'sint8)
     (#x02 'sint16)
     (#x04 'sint32)
     (#x08 'sint64)
     (#x11 'uint8)
     (#x12 'uint16)
     (#x14 'uint32)
     (#x18 'uint64)
     (#x24 'float32)
     (#x28 'float64)))
  (samples (vector (slot-value format) *)))

(define-io-backend-functions io-stream audio)

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

(define-io-alias halftoning-algorithm
  (case uint16
    (0 NIL)
    (1 :error-diffusion)
    (2 :panda)
    (3 :super-circle)))

(define-io-structure os22xbitmapheader
  (header bitmapinfoheader)
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
            #++(52 bitmapv2infoheader)
            #++(56 bitmapv3infoheader)
            (64 os22xbitmapheader)
            #++(108 bitmapv4header)
            #++(124 bitmapv5header))))

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
