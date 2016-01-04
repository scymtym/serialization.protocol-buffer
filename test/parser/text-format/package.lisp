;;;; package.lisp --- Package definition for unit tests of the parser.text-format module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer.parser.text-format.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:serialization.protocol-buffer.parser.text-format)

  (:import-from #:parser.common-rules.test
   #:parses-are)

  (:import-from #:serialization.protocol-buffer.parser.text-format
   #:value/scalar #:list/scalar
                  #:list/message)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for parser.text-format module."))

(cl:in-package #:serialization.protocol-buffer.parser.text-format.test)

(def-suite :serialization.protocol-buffer.parser.text-format
  :description
  "Unit test suite for the parser.text-format module.")

(defun run-tests ()
  (run! :serialization.protocol-buffer.parser.text-format))

;;; Test utilities

(defmacro define-rule-test (rule &body cases)
  (let ((test-name (symbolicate '#:rule. rule)))
    `(test ,test-name
       ,(format nil "Smoke test for the `~(~A~)' rule." rule)
       (architecture.builder-protocol:with-builder ('list)
         (parses-are (,rule) ,@cases)))))
