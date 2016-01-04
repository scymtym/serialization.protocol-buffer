;;;; package.lisp --- Package definition for unit tests of the parser module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer.parser.test
  (:use
   #:cl
   #:alexandria
   #:fiveam

   #:serialization.protocol-buffer.parser)

  (:import-from #:parser.common-rules.test
   #:parses-are)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the parser module."))

(cl:in-package #:serialization.protocol-buffer.parser.test)

(def-suite :serialization.protocol-buffer.parser
  :description
  "Unit test suite for the parser module.")

(defun run-tests ()
  (run! :serialization.protocol-buffer.parser))

;;; Test utilities

(defmacro define-rule-test (rule &body cases)
  (let ((test-name (symbolicate '#:rule. rule)))
    `(test ,test-name
       ,(format nil "Smoke test for the `~(~A~)' rule." rule)
       (parses-are (,rule) ,@cases))))
