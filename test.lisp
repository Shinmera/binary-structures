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
     (#x01 sint8)
     (#x02 sint16)
     (#x04 sint32)
     (#x08 sint64)
     (#x11 uint8)
     (#x12 uint16)
     (#x14 uint32)
     (#x18 uint64)
     (#x24 float32)
     (#x28 float64)))
  (samples (vector (slot-value format) *)))

(define-io-backend-functions io-stream audio)
