;;;; types.lisp --- Tests for types provided by the main module.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :serialization.protocol-buffer.test)

(in-suite :serialization.protocol-buffer)

(test wire-type-code<->name.smoke
  "Smoke tests for the `wire-type-code->name' and
   `wire-type-name->code' functions."

  (mapc (lambda (case)
          (destructuring-bind (code expected-name) case
            (let* ((name  (wire-type-code->name code))
                   (code2 (wire-type-name->code name)))
              ;; Check name
              (is (typep name 'wire-type/name))
              (is (eql expected-name name))
              ;; Check code
              (is (typep code2 'wire-type/code))
              (is (eql code code2)))))
        '((0 :varint)
          (1 :fixed64)
          (2 :size-delimited)
          (5 :fixed32))))
