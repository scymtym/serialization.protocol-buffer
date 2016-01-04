;;;; serialization.protocol-buffer.parser.asd --- Definition of the serialization.protocol-buffer.parser system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :serialization.protocol-buffer.parser
  :description    "Shared infrastructure for protocol buffer-related parsers."
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
                   (:version :esrap               "0.15")
                   (:version :parser.common-rules "0.2"))

  :components     ((:module     "parser"
                    :pathname   "src/parser"
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "grammar"))))

  :in-order-to    ((test-op (test-op :serialization.protocol-buffer.parser/test))))

(defsystem :serialization.protocol-buffer.parser/test
  :description "Tests for the serialization.protocol-buffer.parser system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  (:alexandria
                (:version :fiveam                               "1.3")

                (:version :parser.common-rules/test             "0.2")

                (:version :serialization.protocol-buffer.parser (:read-file-form "version-string.sexp")))

  :components  ((:module     "parser"
                 :pathname   "test/parser"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "grammar")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :serialization.protocol-buffer.parser/test))))
  (uiop:symbol-call '#:serialization.protocol-buffer.parser.test
                    '#:run-tests))
