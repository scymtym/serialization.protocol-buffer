;;;; grammar.lisp --- Protocol buffer descriptor grammar.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; See https://developers.google.com/protocol-buffers/docs/reference/proto2-spec
;;;; See https://developers.google.com/protocol-buffers/docs/reference/proto3-spec

(cl:in-package #:serialization.protocol-buffer.parser.proto)

;;; Utilities

(defun only-in-version-2 (production)
  (if (= *version* 2)
      production
      (values
       nil
       (make-condition
        'simple-error
        :format-control   "~@<The label \"~(~A~)\" is not supported in ~
                           protocol buffer syntax version ~A.~@:>"
        :format-arguments (list production *version*)))))

(defrule ignored-semicolon
    delimiter-semicolon
  (:constant nil)
  (:error-report nil))

(defmacro define-named-block-rule ((name keyword-rule name-rule kind element-relation)
                                   element-expression
                                   &rest options)
  (let+ ((element-rule-name (symbolicate name '#:-element))
         (rule-name         name)
         ((&values element-expression element-multiplicity)
          (if (typep element-expression '(cons (member * +)))
              (values (second element-expression) (first element-expression))
              (values element-expression          '*))))
    `(progn
       (defrule ,element-rule-name
           (or ,element-expression
               comment whitespace+ ignored-semicolon))

       (defrule ,rule-name
           (and ,keyword-rule ,name-rule
                delimiter-{
                (,element-multiplicity ,element-rule-name)
                delimiter-})
         (:destructure (keyword name open content close &bounds start end)
           (declare (ignore keyword open close))
           (bp:node* (,kind :name name :bounds (cons start end))
             (* ,element-relation content)))
         ,@options))))

;;; Comment-related rules

(defrule comment/rest-of-line/same-line
    c-style-comment/rest-of-line/trimmed
  (:lambda (content &bounds start end)
    (bp:node* (:comment :content content :bounds (cons start end))))
  (:error-report nil))

(defrule comment/rest-of-line+newline
    (and comment/rest-of-line/same-line #\Newline)
  (:function first)
  (:error-report :detail))

(defrule comment/delimited
    c-style-comment/delimited/trimmed
  (:lambda (content &bounds start end)
    (bp:node* (:comment :content content :bounds (cons start end))))
  (:error-report :detail))

(defrule toplevel-comment
    (or comment/rest-of-line/same-line comment/delimited))

(defrule comment
    (or comment/rest-of-line+newline comment/delimited))

;;; Types

(macrolet
    ((define-type-rules ()
       (let+ ((all-rules '())
              (key-rules '())
              ((&flet make-type-rule (type)
                 (let ((rule-name (symbolicate '#:primitive-type- type)))
                   (push rule-name all-rules)
                   (unless (member type '(:float :double))
                     (push rule-name key-rules))
                   `(defrule ,rule-name
                        ,(string-downcase type)
                      (:constant ',type))))))
         `(progn
            ,@(mapcar #'make-type-rule +primitive-types+)

            (defrule/s key-type
                (or ,@key-rules)
              (:lambda (name &bounds start end)
                (bp:node* (:primitive-type :name   name
                                           :bounds (cons start end)))))

            (defrule primitive-type
                (or ,@all-rules)
              (:lambda (name &bounds start end)
                (bp:node* (:primitive-type :name   name
                                           :bounds (cons start end)))))))))
  (define-type-rules))

(defrule compound-type-reference
    (and (? #\.) dotted-identifier)
  (:destructure (dot name &bounds start end)
    (let ((name (cons (if dot :absolute :relative) name)))
      (bp:node* (:type-reference :name name :bounds (cons start end))))))

(defrule/s map-type
    (and keyword-map/?s
         delimiter-</?s
         key-type/?s delimiter-comma/?s type-reference/?s
         delimiter->)
  (:destructure (keyword open key-type comma value-type close
                 &bounds start end)
    (declare (ignore keyword open comma close))
    (bp:node* (:map :bounds (cons start end))
      (1 (:key-type   . 1) key-type)
      (1 (:value-type . 1) value-type))))

(defrule/s type-reference
    (or primitive-type compound-type-reference))

;;; Syntax and import statements

(macrolet ((define (name string value)
             `(progn
                (defun ,name (string)
                  (string= string ,string))

                (defrule ,name
                    (,name string-literal)
                  (:constant ,value)))))
  (define syntax-designator/2 "proto2" 2)
  (define syntax-designator/3 "proto3" 3))

(defrule syntax-designator
    (or syntax-designator/2 syntax-designator/3))

(defun apply-syntax (value)
  (setf *version* value))

(defrule/s applied-syntax
    (apply-syntax syntax-designator))

(defrule statement-syntax
    (and keyword-syntax/?s delimiter-equals/?s applied-syntax/?s
         delimiter-semicolon)
  (:destructure (keyword equals value semicolon &bounds start end)
    (declare (ignore keyword equals semicolon))
    (bp:node* (:syntax :value value :bounds (cons start end)))))

(defrule/s import-mode
    (or keyword-weak     ; optional dependency; may be missing
        keyword-public)) ; allow references to types in transitive
                         ; imports

(defrule statement-import
    (and (or (and keyword-import/s  import-mode/?s)
             (and keyword-import/?s (and)))
         string-literal/?s delimiter-semicolon)
  (:destructure ((keyword mode) value semicolon &bounds start end)
    (declare (ignore keyword semicolon))
    (bp:node* (:import :mode mode :value value :bounds (cons start end)))))

;;; Option

(defrule option-name-start
    (or (and delimiter-\(/?s dotted-identifier/?s delimiter-\))
        (and (and)           (and identifier/?s)  (and)))
  (:function second))

(defrule/s option-name
    (and option-name-start (* (and #\. identifier)))
  (:destructure (first rest)
    (list* first (mapcar #'second rest))))

(defrule option-value
    (or literal identifier)
  (:lambda (value &bounds start end)
    (bp:node* (:value :value value :bounds (cons start end)))))

(defrule/s option-body
    (and option-name/?s delimiter-equals/?s option-value))

(flet ((node (name value bounds)
         (bp:node* (:option :name name :bounds bounds)
           (1 (:value . 1) value))))

  (defrule/s option
      option-body
    (:destructure (name equals value &bounds start end)
      (declare (ignore equals))
      (node name value (cons start end))))

  (defrule statement-option
      (and (or keyword-option/s
               (and keyword-option/?s (& delimiter-\()))
           option-body/?s delimiter-semicolon)
    (:function second)
    (:destructure (name equals value &bounds start end)
      (declare (ignore equals))
      (node name value (cons start end)))))

;;; Fields

(defrule/s label
    (or label-required label-optional keyword-repeated))

(defrule label-none
    (and)
  (:when (>= *version* 3)))

(defrule label-required
    keyword-required
  (:when (= *version* 2)))

(defrule label-optional
    keyword-optional
  (:when (= *version* 2)))

(defrule/s field-name
    identifier)

(defun field-number? (thing)
  (or (typep thing 'serialization.protocol-buffer:field-number)
      (values
       nil
       (make-condition
        'simple-error
        :format-control   "~@<Requested field number ~A is not an ~
                           integer in the range [~D, ~D].~@:>"
        :format-arguments (list thing 0 +most-positive-field-number+)))))

(defrule/s field-number
    (field-number? integer-literal))

(defrule/s field-options
    (and delimiter-[/?s
         option/?s (* (and delimiter-comma/s option/?s))
         delimiter-])
  (:destructure (open first rest close)
    (declare (ignore open close))
    (list* first (mapcar #'second rest))))

(defrule/s field-content
    (and field-name/?s delimiter-equals/?s field-number/?s
         (? field-options/?s)
         delimiter-semicolon))

(defrule field/valid
    (and (or label/s label-none)
         (or map-type/?s type-reference/s)
         field-content)
  (:destructure (label type (name equals number options semicolon)
                 &bounds start end)
    (declare (ignore equals semicolon))
    (bp:node* (:field :name   name
                      :number number
                      :label  label
                      :bounds (cons start end))
      (1 (:type . 1) type)
      (* :option     options))))

(defrule field/required-or-optional
    (and (or keyword-required/s keyword-optional/s)
         (or map-type/?s type-reference/s)
         field-content)
  (:function first))

(defrule field/invalid
    (only-in-version-2 field/required-or-optional)
  (:when (>= *version* 3)))

(defrule field
    (or field/valid field/invalid))

(defrule oneof-field
    (and type-reference/s field-content)
  (:destructure (type (name equals number options semicolon)
                 &bounds start end)
    (declare (ignore equals semicolon))
    (bp:node* (:field :name   name
                      :number number
                      :bounds (cons start end))
      (1 (:type . 1) type)
      (* :option     options))))

(define-named-block-rule
    (oneof keyword-oneof/s identifier/?s :one-of :variant)
  (+ oneof-field))

;;; Reserved

(defrule field-number-range
    (or (and field-number/s keyword-to/s (or field-number keyword-max))
        (and field-number   (and)        (and)))
  (:destructure (lower to upper &bounds start end)
    (declare (ignore to))
    (bp:node* (:field-number-range :lower  lower
                                   :upper  upper
                                   :bounds (cons start end)))))

(defrule/s list-of-field-number-range
    (and field-number-range
         (* (and whitespace* delimiter-comma/?s field-number-range)))
  (:destructure (first rest)
    (list* first (map 'list #'third rest))))

(defrule reserved-field-name
    field-name
  (:lambda (name &bounds start end)
    (bp:node* (:field-name :name name :bounds (cons start end)))))

(defrule/s list-of-field-name
    (and reserved-field-name
         (* (and whitespace* delimiter-comma/?s reserved-field-name)))
  (:destructure (first rest)
    (list* first (map 'list #'third rest))))

(macrolet
    ((define-reserved-rule (name element-rule kind)
       (let ((element-rule/?s (symbolicate element-rule '#:/?s)))
         `(defrule ,name
              (and keyword-reserved/s ,element-rule/?s delimiter-semicolon)
            (:destructure (keyword elements semicolon &bounds start end)
              (declare (ignore keyword semicolon))
              (bp:node* (,kind :bounds (cons start end))
                (* :element elements)))))))

  (define-reserved-rule reservation/field-number
      list-of-field-number-range :reservation/field-number)
  (define-reserved-rule reservation/field-name
      list-of-field-name :reservation/field-name))

(defrule reservation
    (or reservation/field-number reservation/field-name))

;;; Message

(define-named-block-rule
    (message keyword-message/s identifier/?s :message :child)
  (or message enum statement-option field oneof reservation))

;;; Enum

(defun enum-value? (thing)
  (or (typep thing 'serialization.protocol-buffer:enum-value)
      (values
       nil
       (make-condition
        'simple-error
        :format-control   "~@<Requested Enum value ~D is not an integer ~
                           in the range [~D, ~D].~@:>"
        :format-arguments (list thing
                                +most-negative-enum-value+
                                +most-positive-enum-value+)))))

(defrule enum-value
    (and identifier/?s delimiter-equals/?s
         (enum-value? integer-literal/?s)
         (? field-options/?s) delimiter-semicolon)
  (:destructure (name equals value options semicolon &bounds start end)
    (declare (ignore equals semicolon))
    (bp:node* (:enum-value :name name :value value :bounds (cons start end))
      (* :option options))))

(define-named-block-rule
    (enum keyword-enum/s identifier/?s :enum :child)
  (or enum-value statement-option))

;;; Package

(defrule statement-package
    (and keyword-package/s dotted-identifier/?s delimiter-semicolon)
  (:destructure (keyword name semicolon &bounds start end)
    (declare (ignore keyword semicolon))
    (bp:node* (:package :name   name
                        :bounds (cons start end)))))

;;; Root

(defrule proto
    (and (* (or toplevel-comment whitespace+))
         (? statement-syntax)
         (* (or statement-package
                statement-import
                statement-option
                message enum
                toplevel-comment whitespace+ ignored-semicolon)))
  ;; Root production; parses top-level comments, package directives
  ;; and top-level definitions.
  (:destructure (header syntax content &bounds start end)
    (bp:node* (:file :bounds (cons start end))
      (* :child (append header (list syntax) content)))))
