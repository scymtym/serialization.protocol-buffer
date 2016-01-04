;;;; package.lisp --- Package definition for unit tests of the main module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer.test
  (:use
   #:cl
   #:fiveam

   #:serialization.protocol-buffer)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the main module."))

(cl:in-package #:serialization.protocol-buffer.test)

(def-suite :serialization.protocol-buffer
  :description
  "Unit test suite for the main module.")

(defun run-tests ()
  (run! :serialization.protocol-buffer))
