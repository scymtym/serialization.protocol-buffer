;;;; types.lisp --- Types used in the serialization.protocol-buffer system.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:serialization.protocol-buffer)

;;; Wire-types
;;;
;;; Wire-types indicate the fundamental encoding of scheme,
;;; irregardless of the interpretation of the encoded data.
;;;
;;; On this level there are only four encodings:
;;; 1. 32-bit fixed-width
;;; 2. 64-bit fixed-width
;;; 3. variable-width integer
;;; 4. length-delimited data

(defconstant +most-positive-field-number+
  #x1fffffff)

(deftype field-number ()
  `(integer 1 ,+most-positive-field-number+))

(deftype wire-type/code ()
  '(member 0 1 2 5))

(deftype wire-type/name ()
  '(member :varint :fixed64 :size-delimited :fixed32))

(declaim (ftype (function (wire-type/code) wire-type/name)
                wire-type-code->name)
         (ftype (function (wire-type/name) wire-type/code)
                wire-type-name->code)
         (inline wire-type-code->name wire-type-name->code))

(defun wire-type-code->name (code)
  (ecase code
    (0 :varint)
    (1 :fixed64)
    (2 :size-delimited)
    (5 :fixed32)))

(defun wire-type-name->code (name)
  (ecase name
    (:varint         0)
    (:fixed64        1)
    (:size-delimited 2)
    (:fixed32        5)))

;;; Start-codes
;;;
;;; A start code precedes the encoded data of a field and indicates
;;; the field-number and the wire-type of the field.

(deftype start-code/code/cons ()
  '(cons field-number wire-type/code))

(deftype start-code/name/cons ()
  '(cons field-number wire-type/name))

(deftype start-code/number ()
  `(integer 0 ,(logior (ash +most-positive-field-number+ 3) 5)))

;;; Enums

(defconstant +most-negative-enum-value+ #x-80000000)

(defconstant +most-positive-enum-value+ #x7fffffff)

(deftype enum-value ()
  `(integer ,+most-negative-enum-value+ ,+most-positive-enum-value+))

;;; Symbol-designated primitive proto types

(define-constant +primitive-types+
    '(:bool

      :fixed32 :sfixed32
      :fixed64 :sfixed64

      :float   :double

      :int32   :uint32   :sint32
      :int64   :uint64   :sint64

      :string  :bytes)
  :test #'equal)

(deftype primitive-type ()
  `(member ,@+primitive-types+))

(deftype fixed32-type ()
  '(member :float :fixed32 :sfixed32))

(deftype fixed64-type ()
  '(member :double :fixed64 :sfixed64))

(deftype fixed-type ()
  '(or fixed32-type fixed64-type))
