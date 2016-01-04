;;;; grammar.lisp --- Tests for the text format grammar.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :serialization.protocol-buffer.parser.text-format.test)

(in-suite :serialization.protocol-buffer.parser.text-format)

(define-rule-test message
  ;; Some invalid cases.
  (""       nil)
  ("{"      nil)
  ("<"      nil)
  ("["      nil)
  ("foo"    nil)
  ("{foo}"  nil)
  ("{foo:}" nil)

  ;; These are OK.
  ("{}"                  '(:message () :bounds (0 . 2)))
  ("<>"                  '(:message () :bounds (0 . 2)))
  ("{ }"                 '(:message () :bounds (0 . 3)))
  ("< >"                 '(:message () :bounds (0 . 3)))
  (((format nil "{~%}")) '(:message () :bounds (0 . 3)))
  (((format nil "<~%>")) '(:message () :bounds (0 . 3)))

  ("{foo:1}"
   '(:message (:field (((:field (:value (((:literal ()
                                                    :value  1
                                                    :bounds (5 . 6)))))
                                :name "foo" :bounds (1 . 6)))))
     :bounds (0 . 7)))

  ("{foo:1 bar<>}"
   '(:message (:field (((:field (:value (((:literal ()
                                                    :value  1
                                                    :bounds (5 . 6)))))
                                :name "foo" :bounds (1 . 6)))
                       ((:field (:value (((:message () :bounds (10 . 12)))))
                                :name "bar" :bounds (7 . 12)))))
     :bounds (0 . 13))))

(define-rule-test field
  ;; Some invalid cases.
  (""       nil)
  ("foo"    nil)
  ("foo:"   nil)
  ("foo::1" nil)
  ("foo:[]" nil) ; empty list value is not allowed

  ;; Some valid cases.
  ("foo:false"
   '(:field (:value (((:literal ()
                                :value  nil
                                :bounds (4 . 9)))))
     :name "foo" :bounds (0 . 9)))

  ("foo:1"
   '(:field (:value (((:literal ()
                                :value  1
                                :bounds (4 . 5)))))
     :name "foo" :bounds (0 . 5)))

  ("foo:[1]"
   '(:field (:value (((:literal ()
                                :value  1
                                :bounds (5 . 6)))))
     :name "foo" :bounds (0 . 7)))

  ("foo:[1,2]"
   '(:field (:value (((:literal ()
                                :value  1
                                :bounds (5 . 6)))
                     ((:literal ()
                                :value  2
                                :bounds (7 . 8)))))
     :name "foo" :bounds (0 . 9)))

  ("foo:{}"
   '(:field (:value (((:message () :bounds (4 . 6)))))
     :name "foo" :bounds (0 . 6)))

  ("foo:<>"
   '(:field (:value (((:message () :bounds (4 . 6)))))
     :name "foo" :bounds (0 . 6)))

  ("foo{}"
   '(:field (:value (((:message () :bounds (3 . 5)))))
     :name "foo" :bounds (0 . 5)))

  ("foo<>"
   '(:field (:value (((:message () :bounds (3 . 5)))))
     :name "foo" :bounds (0 . 5))))

(define-rule-test list/scalar
  ;; Some syntax errors.
  (""      nil)
  ("[]"    nil) ; empty list value is not allowed
  ("[1 2]" nil) ; missing separator

  ;; Some valid cases.
  ("[1]"   '((:literal () :value 1 :bounds (1 . 2))))
  ("[1,2]" '((:literal () :value 1 :bounds (1 . 2))
             (:literal () :value 2 :bounds (3 . 4)))))

(define-rule-test list/message
  ;; Some syntax errors.
  (""        nil)
  ("[]"      nil) ; empty list value is not allowed
  ("[<> <>]" nil) ; missing separator

  ;; Some valid cases.
  ("[<>]"    '((:message () :bounds (1 . 3))))
  ("[<>,{}]" '((:message () :bounds (1 . 3))
               (:message () :bounds (4 . 6)))))

(define-rule-test value/scalar
  ;; Some syntax errors.
  (""    nil)
  ("foo" nil)
  ("[]"  nil)
  ("{}"  nil)
  ("\""  nil)

  ;; Some valid cases.
  ("false"        '(:literal () :value  nil   :bounds (0 . 5)))
  ("true"         '(:literal () :value  t     :bounds (0 . 4)))
  ("1"            '(:literal () :value  1     :bounds (0 . 1)))
  ("-1"           '(:literal () :value  -1    :bounds (0 . 2)))
  ("+1"           '(:literal () :value  1     :bounds (0 . 2)))
  ("1.0"          '(:literal () :value  1.0f0 :bounds (0 . 3)))
  ("\"foo\\000\"" `(:literal ()
                             :value  ,(format nil "foo~C" #\Nul)
                             :bounds (0 . 9))))
