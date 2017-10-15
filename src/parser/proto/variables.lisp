;;;; variables.lisp --- Variables used by the parser.proto module.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:serialization.protocol-buffer.parser.proto)

(declaim (type (member 2 3) *version*))
(defvar *version* 2
  "The currently assumed protocol buffer syntax version.

   Updated when syntax statements are encountered.")
