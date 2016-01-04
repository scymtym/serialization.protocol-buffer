;;;; grammar.lisp --- Tests for the grammar rules of the parser module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:serialization.protocol-buffer.parser.test)

(in-suite :serialization.protocol-buffer.parser)

(define-rule-test literal
  ("true"                    t)
  ("false"                   nil nil t)

  (".1"                      .1f0)
  ("1.0"                     1f0)
  ("1.0e5"                   1f5)
  ("1e-10"                   1f-10)

  ("-1"                      -1)
  ("0"                       0)
  ("1"                       1)

  ("'foo'"                   "foo")
  ("'f\\'o\"\"o'"            "f'o\"\"o")

  ("\"foo\""                 "foo")
  ("\"f'o\\\"\\\"o\""        "f'o\"\"o")

  ("'baz\\x20'"              "baz ")
  ("\"baz\\x20\""            "baz ")
  ("'baz\\n'"                (format nil "baz~%"))

  ("'a''b'"                  "ab")
  ("'a' 'b'"                 "ab")
  (((format nil "'a'~%'b'")) "ab"))

(define-rule-test identifier
  ;; Some syntax errors.
  ("1"   nil)
  ("+"   nil)
  ("?"   nil)
  ("a b" "a" 1 t)

  ;; Some valid cases.
  ("a"   "a")
  ("A"   "A")
  ("_"   "_")
  ("a1"  "a1"))

(define-rule-test dotted-identifier
  ;; Some syntax errors.
  (""     nil)
  ("."    nil)
  ("a."   '("a") 1 t)
  (".a"   nil)
  ("a..b" '("a") 1 t)

  ;; Some valid cases
  ("a"    '("a"))
  ("a.b"  '("a" "b")))
