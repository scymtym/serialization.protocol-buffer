;;;; package.lisp --- Package definition for shared parser rules.
;;;;
;;;; Copyright (C) 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:serialization.protocol-buffer.parser
  (:use
   #:cl
   #:alexandria
   #:let-plus

   #:esrap
   #:parser.common-rules)

  (:shadow
   #:integer-literal)

  ;; Whitespace
  (:export
   #:whitespace
   #:skippable #:skippable?)

  (:export
   #:delimiter-comma     #:delimiter-comma/s     #:delimiter-comma/?s
   #:delimiter-semicolon #:delimiter-semicolon/s #:delimiter-semicolon/?s
   #:delimiter-colon     #:delimiter-colon/s     #:delimiter-colon/?s
   #:delimiter-equals    #:delimiter-equals/s    #:delimiter-equals/?s

   #:delimiter-<         #:delimiter-</s         #:delimiter-</?s
   #:delimiter->         #:delimiter->/s         #:delimiter->/?s

   #:delimiter-{         #:delimiter-{/s         #:delimiter-{/?s
   #:delimiter-}         #:delimiter-}/s         #:delimiter-}/?s

   #:delimiter-[         #:delimiter-[/s         #:delimiter-[/?s
   #:delimiter-]         #:delimiter-]/s         #:delimiter-]/?s

   #:delimiter-\(        #:delimiter-\(/s        #:delimiter-\(/?s
   #:delimiter-\)        #:delimiter-\)/s        #:delimiter-\)/?s)

  (:export
   #:keyword-syntax   #:keyword-syntax/s   #:keyword-syntax/?s
   #:keyword-import   #:keyword-import/s   #:keyword-import/?s
   #:keyword-weak     #:keyword-weak/s     #:keyword-weak/?s
   #:keyword-public   #:keyword-public/s   #:keyword-public/?s
   #:keyword-option   #:keyword-option/s   #:keyword-option/?s

   #:keyword-package  #:keyword-package/s  #:keyword-package/?s
   #:keyword-message  #:keyword-message/s  #:keyword-message/?s
   #:keyword-enum     #:keyword-enum/s     #:keyword-enum/?s
   #:keyword-service  #:keyword-service/s  #:keyword-service/?s

   #:keyword-required #:keyword-required/s #:keyword-required/?s
   #:keyword-optional #:keyword-optional/s #:keyword-optional/?s
   #:keyword-repeated #:keyword-repeated/s #:keyword-repeated/?s

   #:keyword-oneof    #:keyword-oneof/s    #:keyword-oneof/?s

   #:keyword-to       #:keyword-to/s       #:keyword-to/?s
   #:keyword-max      #:keyword-max/s      #:keyword-max/?s
   #:keyword-reserved #:keyword-reserved/s #:keyword-reserved/?s

   #:keyword-map      #:keyword-map/s      #:keyword-map/?s

   #:keyword-rpc      #:keyword-rpc/s      #:keyword-rpc/?s
   #:keyword-returns  #:keyword-returns/s  #:keyword-returns/?s
   #:keyword-stream   #:keyword-stream/s   #:keyword-stream/?s)

  ;; Literals
  (:export
   #:integer-literal #:integer-literal/s   #:integer-literal/?s
   #:string-literal  #:string-literal/s    #:string-literal/?s
   #:literal         #:literal/s           #:literal/?s)

  ;; Identifiers
  (:export
   #:identifier        #:identifier/s        #:identifier/?s
   #:dotted-identifier #:dotted-identifier/s #:dotted-identifier/?s)

  (:documentation
   "Common rules used by both parser sub-packages."))
