;;;; grammar.lisp --- Shared grammar rules.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; See https://developers.google.com/protocol-buffers/docs/reference/proto2-spec
;;;; See https://developers.google.com/protocol-buffers/docs/reference/proto3-spec

(cl:in-package #:serialization.protocol-buffer.parser)

;;; Whitespace stuff

(defvar *comment-rule* nil)

(defun parse-comment (text position end)
  (if-let ((rule *comment-rule*))
    (parse rule text :start position :end end :raw t)
    (values nil position nil)))

(defrule skippable?
    (* (or whitespace #'parse-comment))
  (:constant nil)
  (:error-report nil))

(defrule skippable
    (+ (or whitespace #'parse-comment))
  (:constant nil))

;;; Delimiters and keywords

(macrolet ((define-delimiter (name character)
             (let ((rule-name (symbolicate '#:delimiter- name)))
               `(defrule/s ,rule-name ,character
                  (:error-report :detail)))))
  (define-delimiter comma     #\,)
  (define-delimiter semicolon #\;)
  (define-delimiter colon     #\:)
  (define-delimiter equals    #\=)
  (define-delimiter <         #\<)
  (define-delimiter >         #\>)
  (define-delimiter {         #\{)
  (define-delimiter }         #\})
  (define-delimiter [         #\[)
  (define-delimiter ]         #\])
  (define-delimiter \(        #\()
  (define-delimiter \)        #\)))

(macrolet ((define-keyword (name)
             (let ((rule-name (symbolicate '#:keyword- name))
                   (string    (string-downcase name))
                   (value     (make-keyword name)))
               `(defrule/s ,rule-name ,string
                  (:constant ,value)))))
  (define-keyword syntax)
  (define-keyword import)
  (define-keyword weak)
  (define-keyword public)
  (define-keyword option)

  (define-keyword package)
  (define-keyword message)
  (define-keyword enum)

  (define-keyword required)
  (define-keyword optional #+later version-2)
  (define-keyword repeated)

  (define-keyword oneof)

  (define-keyword to)
  (define-keyword max)
  (define-keyword reserved #+later version-3)

  (define-keyword map #+later version-3))

;;; Literals

(defrule/s integer-literal parser.common-rules:integer-literal)

(defrule string-literal-part
    (or string-literal/double-quotes
        string-literal/single-quotes))

(defrule/s string-literal
    (and string-literal-part (* (and skippable? string-literal-part)))
  (:destructure (first rest)
    (if rest
        (apply #'concatenate 'string first (mapcar #'second rest))
        first)))

(defrule/s literal
    (or boolean-literal/lower-case
        float-literal
        integer-literal
        string-literal))

;;; Identifiers

(defrule alpha-or-underscore
    (character-ranges (#\a #\z) (#\A #\Z) #\_))

(defrule/s identifier
    (and alpha-or-underscore
         (* (or alpha-or-underscore (character-ranges (#\0 #\9)))))
  (:text t))

(defrule/s dotted-identifier
    (and identifier (* (and #\. identifier)))
  (:destructure (first rest)
    (list* first (mapcar #'second rest))))
