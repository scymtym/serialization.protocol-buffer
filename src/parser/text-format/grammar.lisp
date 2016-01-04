;;;; grammar.lisp --- Grammar for the protocol buffer text format.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Grammar for protocol buffer TextFormat.
;;;;
;;;; Mainly based on
;;;; protobuf-3.0.0/python/google/protobuf/text_format.py
;;;;
;;;; Current limitations
;;;; * No extensions
;;;; * No field numbers instead of names
;;;; * No short Boolean values "t" and "f"
;;;; * No float infinities and NaNs

(cl:in-package #:serialization.protocol-buffer.parser.text-format)

(defrule/s message
    (or (and delimiter-</?s (* field/?s) delimiter->)
        (and delimiter-{/?s (* field/?s) delimiter-}))
  (:function second)
  (:lambda (fields &bounds start end)
    (node* (:message :bounds (cons start end))
      (* :field fields))))

(defrule/s field
    (or field/message field/scalar)
  (:destructure (name colon values &bounds start end)
    (declare (ignore colon))
    (node* (:field :name name :bounds (cons start end))
      (* :value values))))

(defrule field/message
    (and identifier/?s (? delimiter-colon/?s)
         (or list/message (and message))))

(defrule field/scalar
    (and identifier/?s delimiter-colon/?s
         (or list/scalar (and value/scalar))))

(macrolet
    ((define-list (suffix
                   &key
                   (value-name (symbolicate '#:value/ suffix '#:/?s)))
       (let ((list-name   (symbolicate '#:list/ suffix))
             (values-name (symbolicate '#:values/ suffix '#:/?s)))
         `(progn
            (defrule ,list-name
                (and delimiter-[/?s ,values-name delimiter-])
              (:function second))

            (defrule ,values-name
                (and ,value-name (* (and delimiter-comma/?s ,value-name)))
              (:destructure (first rest)
                (when first (list* first (mapcar #'second rest)))))))))
  (define-list scalar)
  (define-list message :value-name message/?s))

(defrule/s value/scalar
    literal
  (:lambda (value &bounds start end)
    (node* (:literal :value value :bounds (cons start end)))))
