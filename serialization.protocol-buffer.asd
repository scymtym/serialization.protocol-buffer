;;;; serialization.protocol-buffer.asd --- System definition for the serialization.protocol-buffer system.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :serialization.protocol-buffer
  :description    "A pure Common Lisp protocol buffer compiler."
  :license        "LLGPLv3" ; see COPYING file for details

  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :mailto         "jmoringe@techfak.uni-bielefeld.de"
  :homepage       "https://github.com/scymtym/serialization.protocol-buffer"
  :bug-tracker    "https://github.com/scymtym/serialization.protocol-buffer/issues"
  :source-control (:git "https://github.com/scymtym/serialization.protocol-buffer.git")

  :version        (:read-file-form "version-string.sexp")
  :depends-on     (:alexandria)

  :components     ((:module     "src"
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "types"))))

  :in-order-to    ((test-op (test-op :serialization.protocol-buffer/test))))

(defsystem :serialization.protocol-buffer/test
  :description "Tests for serialization.protocol-buffer system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version :fiveam                        "1.3")
                (:version :serialization.protocol-buffer (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :serialization.protocol-buffer/test))))
  (uiop:symbol-call '#:serialization.protocol-buffer.test
                    '#:run-tests))
