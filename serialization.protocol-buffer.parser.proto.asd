;;;; serialization.protocol-buffer.parser.proto.asd --- Definition of the serialization.protocol-buffer.parser.proto system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :serialization.protocol-buffer.parser.proto
  :description    "Parser for the protocol buffer data definition DSL."
  :license        "LLGPLv3" ; see COPYING file for details

  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :mailto         "jmoringe@techfak.uni-bielefeld.de"
  :homepage       "https://github.com/scymtym/serialization.protocol-buffer"
  :bug-tracker    "https://github.com/scymtym/serialization.protocol-buffer/issues"
  :source-control (:git "https://github.com/scymtym/serialization.protocol-buffer.git")

  :version        (:read-file-form "version-string.sexp")
  :depends-on     (:alexandria
                   :split-sequence
                   :let-plus
                   :more-conditions

                   :esrap
                   :architecture.builder-protocol

                   :serialization.protocol-buffer
                   :serialization.protocol-buffer.parser)

  :components     ((:module     "parser-proto"
                    :pathname   "src/parser/proto"
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "variables")
                                 (:file       "grammar"))))

  :in-order-to    ((test-op (test-op :serialization.protocol-buffer.parser.proto/test))))

(defsystem :serialization.protocol-buffer.parser.proto/test
  :description "Tests for the serialization.protocol-buffer.parser.proto system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version :fiveam                                     "1.3")

                (:version :parser.common-rules/test                   "0.2")

                (:version :serialization.protocol-buffer.parser.proto (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/parser/proto"
                 :serial     t
                 :components ((:file     "package")
                              (:file     "grammar")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :serialization.protocol-buffer.parser.proto/test))))
  (uiop:symbol-call '#:serialization.protocol-buffer.parser.proto.test
                    '#:run-tests))
