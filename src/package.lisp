;;;; package.lisp --- Package definition for the serialization.protocol-buffer system.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer
  (:use
   #:cl
   #:alexandria)

  ;; Wire types
  (:export
   #:+most-positive-field-number+

   #:field-number

   #:wire-type/code
   #:wire-type/name

   #:wire-type-code->name
   #:wire-type-name->code)

  ;; Start codes
  (:export
   #:start-code/code/cons
   #:start-code/name/cons
   #:start-code/number)

  ;; Enums
  (:export
   #:+most-negative-enum-value+ #:+most-positive-enum-value+
   #:enum-value)

  ;; Primitive types
  (:export
   #:+primitive-types+

   #:primitive-type)

  (:documentation
   "Main package of the serialization.protocol-buffer system.

    Mainly contains basic types such as field numbers and wire-types,
    the corresponding limits and mappings to symbolic
    representations."))
