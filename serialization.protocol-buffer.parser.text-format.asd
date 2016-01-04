;;;; serialization.protocol-buffer.parser.text-format.asd --- Definition of the serialization.protocol-buffer.parser.text-format system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :serialization.protocol-buffer.parser.text-format
  :description    "Parser for the protocol buffer text format."
  :license        "LLGPLv3" ; see COPYING file for details

  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :mailto         "jmoringe@techfak.uni-bielefeld.de"
  :homepage       "https://github.com/scymtym/serialization.protocol-buffer"
  :bug-tracker    "https://github.com/scymtym/serialization.protocol-buffer/issues"
  :source-control (:git "https://github.com/scymtym/serialization.protocol-buffer.git")

  :version        (:read-file-form "version-string.sexp")
  :depends-on     (:alexandria
                   :let-plus

                   (:version :esrap                                "0.15")
                   :architecture.builder-protocol

                   (:version :serialization.protocol-buffer        (:read-file-form "version-string.sexp"))
                   (:version :serialization.protocol-buffer.parser (:read-file-form "version-string.sexp")))

  :components     ((:module     "parser-text-format"
                    :pathname   "src/parser/text-format"
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "grammar"))))

  :in-order-to    ((test-op (test-op :serialization.protocol-buffer.parser.text-format/test))))

(defsystem :serialization.protocol-buffer.parser.text-format/test
  :description "Tests for the serialization.protocol-buffer.parser.text-format system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  (:alexandria
                (:version :fiveam                                           "1.3")
                :parser.common-rules/test

                (:version :serialization.protocol-buffer.parser.text-format (:read-file-form "version-string.sexp")))

  :components  ((:module     "parser-text-format"
                 :pathname   "test/parser/text-format"
                 :serial t
                 :components ((:file       "package")
                              (:file       "grammar")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :serialization.protocol-buffer.parser.text-format/test))))
  (uiop:symbol-call '#:serialization.protocol-buffer.parser.text-format.test
                    '#:run-tests))
