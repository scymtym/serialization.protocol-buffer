;;;; package.lisp --- Package definition for unit tests of the parser.proto module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer.parser.proto.test
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:fiveam

   #:serialization.protocol-buffer.parser.proto)

  (:import-from #:parser.common-rules.test
   #:parses-are)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the parser.proto module."))

(cl:in-package #:serialization.protocol-buffer.parser.proto.test)

(def-suite :serialization.protocol-buffer.parser.proto
  :description
  "Unit test suite for the parser.proto module.")

(defun run-tests ()
  (run! :serialization.protocol-buffer.parser.proto))

;;; Test utilities

(defmacro define-rule-test (rule-and-options &body cases)
  (let+ (((rule &key version) (ensure-list rule-and-options))
         (test-name (apply #'symbolicate '#:rule. rule
                           (when version
                             (list '#:@ (princ-to-string version))))))
    `(test ,test-name
       ,(format nil "Smoke test for the `~(~A~)' rule." rule)

       (architecture.builder-protocol:with-builder ('list)
         (let ((serialization.protocol-buffer.parser::*comment-rule* 'comment)
               ,@(when version
                   `((serialization.protocol-buffer.parser.proto::*version* ,version))))
           (parses-are (,rule) ,@cases))))))
