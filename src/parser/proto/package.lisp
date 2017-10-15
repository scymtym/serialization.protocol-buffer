;;;; package.lisp --- Package definition for the parser.proto module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer.parser.proto
  (:use
   #:cl
   #:alexandria
   #:split-sequence
   #:let-plus
   #:more-conditions

   #:esrap
   #:parser.common-rules

   #:serialization.protocol-buffer
   #:serialization.protocol-buffer.parser)

  (:shadow
   #:field-number
   #:enum-value
   #:primitive-type)

  (:shadowing-import-from #:serialization.protocol-buffer.parser
   #:integer-literal)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  ;; Variables
  (:export
   #:*version*)

  ;; Rules
  (:export
   #:toplevel-comment
   #:comment

   #:primitive-type
   #:compound-type-reference
   #:map-type
   #:type-reference

   #:statement-syntax
   #:statement-import

   #:option-name
   #:option
   #:statement-option

   #:field
   #:oneof

   #:reservation

   #:message

   #:enum-value
   #:enum

   #:statement-package

   #:proto)

  (:documentation
   "Rules for parsing the textual protocol buffer descriptor syntax.

    The primary entry-point is the `parse' function. A builder protocol,
    consisting of `make-*' and `add-child' functions can be implemented to
    receive parsing results.

    The following things are not supported:
    * service keyword
    * rpc keyword
    * extend keyword
    * group keyword

    The grammar rules are based on
    * https://developers.google.com/protocol-buffers/docs/reference/proto2-spec
    * https://developers.google.com/protocol-buffers/docs/reference/proto3-spec"))
