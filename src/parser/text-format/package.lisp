;;;; package.lisp --- Package definition for the parser.text-format module.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer.parser.text-format
  (:use
   #:cl
   #:alexandria

   #:esrap

   #:serialization.protocol-buffer.parser)

  (:import-from #:parser.common-rules
   #:defrule/s)

  (:import-from #:architecture.builder-protocol
   #:node*)

  ;; Rules
  (:export
   #:field
   #:message)

  (:documentation
   "This package contains a parser for the protocol buffer text
    format."))
