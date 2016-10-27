;;;; grammar.lisp --- Tests for the proto grammar.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :serialization.protocol-buffer.parser.proto.test)
(in-suite :serialization.protocol-buffer.parser.proto)

(define-rule-test toplevel-comment
  (""         nil)
  (" "        nil)
  ("a"        nil)
  ("/"        nil)
  ("/*"       nil)

  ("//"       '(:comment () :content ""     :bounds (0 . 2)))
  ("// "      '(:comment () :content " "    :bounds (0 . 3)))
  ("// //"    '(:comment () :content " //"  :bounds (0 . 5)))
  ("/**/"     '(:comment () :content ""     :bounds (0 . 4)))
  ("/* /* */" '(:comment () :content " /* " :bounds (0 . 8)))
  ("/* // */" '(:comment () :content " // " :bounds (0 . 8))))

(define-rule-test map-type
  ("map"                  nil)
  ("map<"                 nil)
  ("map<>"                nil)
  ("map<,>"               nil)
  ("map<double,string>"   nil)

  ("map<bool/**/,string>" '(:map
                            ((:value-type . 1) (((:primitive-type
                                                  ()
                                                  :name   :string
                                                  :bounds (13 . 19))))
                             (:key-type   . 1) (((:primitive-type
                                                  ()
                                                  :name   :bool
                                                  :bounds (4 . 8)))))
                            :bounds (0 . 20)))
  ("map<string, string>"  '(:map
                            ((:value-type . 1) (((:primitive-type
                                                  ()
                                                  :name   :string
                                                  :bounds (12 . 18))))
                             (:key-type   . 1) (((:primitive-type
                                                  ()
                                                  :name   :string
                                                  :bounds (4 . 10)))))
                            :bounds (0 . 19))))

(define-rule-test type-reference
  (""         nil)
  ("1"        nil)

  ("bool"     '(:primitive-type () :name :bool   :bounds (0 . 4)))
  ("int32"    '(:primitive-type () :name :int32  :bounds (0 . 5)))
  ("uint32"   '(:primitive-type () :name :uint32 :bounds (0 . 6)))
  ("sint32"   '(:primitive-type () :name :sint32 :bounds (0 . 6)))
  ("double"   '(:primitive-type () :name :double :bounds (0 . 6)))
  ("string"   '(:primitive-type () :name :string :bounds (0 . 6)))

  ("Foo"      '(:type-reference () :name (:relative "Foo") :bounds (0 . 3)))
  ("foo.Bar"  '(:type-reference ()
                :name   (:relative "foo" "Bar")
                :bounds (0 . 7)))
  (".foo.Bar" '(:type-reference ()
                :name   (:absolute "foo" "Bar")
                :bounds (0 . 8))))

(define-rule-test statement-syntax
  (""                       nil)
  ("syntax"                 nil)
  ("syntax foo"             nil)
  ("syntax \"foo\""         nil)
  ("syntax = \"proto2\""    nil)
  ("syntax = \"proto1\";"   nil)

  ("syntax=\"proto2\";"     '(:syntax () :value 2 :bounds (0 . 16)))
  ("syntax = \"proto2\" ;"  '(:syntax () :value 2 :bounds (0 . 19)))
  ("syntax = \"proto3\";"   '(:syntax () :value 3 :bounds (0 . 18)))
  ("syntax/**/=\"proto3\";" '(:syntax () :value 3 :bounds (0 . 20)))
  ("syntax = \"pro\"'to3';" '(:syntax () :value 3 :bounds (0 . 20))))

(define-rule-test statement-import
  (""                       nil)
  ("import"                 nil)
  ("importweak"             nil)
  ("import\"foo\""          nil)
  ("importweak\"foo\""      nil)

  ("import weak\"foo\";"    '(:import
                              ()
                              :mode :weak :value "foo" :bounds (0 . 17)))
  ("import public \"foo\";" '(:import
                              ()
                              :mode :public :value "foo" :bounds (0 . 20)))
  ("import\"foo\";"         '(:import
                              ()
                              :mode nil :value "foo" :bounds (0 . 12)))
  ("import/**/\"foo\";"     '(:import
                              ()
                              :mode nil :value "foo" :bounds (0 . 16)))
  ("import\"foo\"'bar';"    '(:import
                              ()
                              :mode nil :value "foobar" :bounds (0 . 17))))

(define-rule-test option-name
  ("(foo)bar"          '(("foo")) 5 t)
  ("((foo)).bar"       nil)

  ("foo"               '(("foo")))
  ("foo.bar"           '(("foo") "bar"))
  ("(foo)"             '(("foo")))
  ("(foo.bar)"         '(("foo" "bar")))
  ("(foo.bar).baz"     '(("foo" "bar") "baz"))
  ("(foo.bar).baz.fez" '(("foo" "bar") "baz" "fez")))

(define-rule-test statement-option
  (""                   nil)
  ("option"             nil)
  ("option foo"         nil)
  ("option foo = 1"     nil)
  ("optionfoo = 1"      nil)


  ("option foo/**/= 1;" '(:option
                          ((:value . 1) (((:value () :value 1 :bounds (16 . 17)))))
                          :name (("foo")) :bounds (0 . 18)))
  ("option foo = true;" '(:option
                          ((:value . 1) (((:value () :value t :bounds (13 . 17)))))
                          :name (("foo")) :bounds (0 . 18)))
  ("option(foo)=1;"     '(:option
                          ((:value . 1) (((:value () :value 1 :bounds (12 . 13)))))
                          :name (("foo")) :bounds (0 . 14))))

(define-rule-test (field :version 2)
  (""                          nil)
  ("required"                  nil)
  ("optional"                  nil)
  ("repeated"                  nil)
  ("required string"           nil)
  ("optional string"           nil)
  ("repeated string"           nil)
  ("required string foo"       nil)
  ("optional string foo"       nil)
  ("repeated string foo"       nil)
  ("required string foo = 1"   nil)
  ("optional string foo = 1"   nil)
  ("repeated string foo = 1"   nil)

  ("required string foo = -1;" nil)
  ("required string foo = 0;"  nil)
  (((format nil "required string foo = ~D;"
            (1+ serialization.protocol-buffer:+most-positive-field-number+)))
   nil)

  ("required string foo = 1;"
   '(:field
     ((:type . 1) (((:primitive-type () :name :string :bounds (9 . 15)))))
     :name "foo" :number 1 :label :required :bounds (0 . 24)))
  ("optional string foo = 1;"
   '(:field
     ((:type . 1) (((:primitive-type () :name :string :bounds (9 . 15)))))
     :name "foo" :number 1 :label :optional :bounds (0 . 24)))
  ("repeated string foo = 1;"
   '(:field
     ((:type . 1) (((:primitive-type () :name :string :bounds (9 . 15)))))
     :name "foo" :number 1 :label :repeated :bounds (0 . 24))))

(define-rule-test (field :version 3)
  (""                         nil)
  ("repeated"                 nil)
  ("string"                   nil)
  ("repeated string"          nil)
  ("string foo"               nil)
  ("repeated string foo"      nil)
  ("string foo = 1"           nil)
  ("repeated string foo = 1"  nil)

  ("string foo = -1;"         nil)
  ("string foo = 0;"          nil)
  (((format nil "string foo = ~D;"
            (1+ serialization.protocol-buffer:+most-positive-field-number+)))
   nil)

  ("required string foo = 0;" nil)
  ("optional string foo = 0;" nil)

  ("string foo = 1;"
   '(:field
     ((:type . 1) (((:primitive-type () :name :string :bounds (0 . 6)))))
     :name "foo" :number 1 :label nil :bounds (0 . 15)))
  ("repeated string foo = 1;"
   '(:field
     ((:type . 1) (((:primitive-type () :name :string :bounds (9 . 15)))))
     :name "foo" :number 1 :label :repeated :bounds (0 . 24))))

(define-rule-test oneof
  (""                                       nil)
  ("oneof"                                  nil)
  ("oneof foo"                              nil)
  ("oneof foo {"                            nil)
  ("oneof foo {}"                           nil)
  ("oneof foo { string bar = 1 }"           nil)
  ("oneof foo { required string bar = 1; }" nil)
  ("oneof foo { optional string bar = 1; }" nil)
  ("oneof foo { repeated string bar = 1; }" nil)

  ("oneof foo { string bar = 1; }"
   '(:one-of
     (:variant
      (((:field
         ((:type . 1) (((:primitive-type () :name :string :bounds (12 . 18)))))
         :name "bar" :number 1 :bounds (12 . 27) ))))
     :name "foo" :bounds (0 . 29))))

(define-rule-test reservation
  ("reserved"     nil)
  ("reserved 1"   nil)
  ("reserved foo" nil)

  ("reserved 0"   nil)

  ("reserved 1;"
   '(:reservation/field-number
     (:element (((:field-number-range
                  ()
                  :lower 1 :upper nil :bounds (9 . 10)))))
     :bounds (0 . 11)))
  ("reserved 1 to 10;"
   '(:reservation/field-number
     (:element (((:field-number-range
                  ()
                  :lower 1 :upper 10 :bounds (9 . 16)))))
     :bounds (0 . 17)))
  ("reserved 1 to max;"
   '(:reservation/field-number
     (:element (((:field-number-range
                  ()
                  :lower 1 :upper :max :bounds (9 . 17)))))
     :bounds (0 . 18)))
  ("reserved 1 to max, 2 to 10;"
   '(:reservation/field-number
     (:element (((:field-number-range
                  ()
                  :lower 1 :upper :max :bounds (9 . 17)))
                ((:field-number-range
                  ()
                  :lower 2 :upper 10 :bounds (19 . 26)))))
     :bounds (0 . 27)))

  ("reserved foo;"
   '(:reservation/field-name
     (:element (((:field-name () :name "foo" :bounds (9 . 12)))))
     :bounds (0 . 13)))
  ("reserved foo, bar;"
   '(:reservation/field-name
     (:element (((:field-name () :name "foo" :bounds (9 . 12)))
                ((:field-name () :name "bar" :bounds (14 . 17)))))
     :bounds (0 . 18))))

(define-rule-test message
  (""                     nil)
  ("message"              nil)
  ("message Foo"          nil)
  ("message Foo {"        nil)

  ("message Foo {}"       '(:message () :name "Foo" :bounds (0 . 14)))
  ("message/**/Foo/**/{}" '(:message () :name "Foo" :bounds (0 . 20)))
  ("message Foo { ; ;}"   '(:message () :name "Foo" :bounds (0 . 18)))

  ("message Foo { // bar
    }"
   '(:message
     (:child
      (((:comment () :content " bar" :bounds (14 . 20)))))
     :name "Foo" :bounds (0 . 26)))

  ("message Foo {
      message Bar {}
    }"
   '(:message
     (:child
      (((:message () :name "Bar" :bounds (20 . 34)))))
     :name "Foo" :bounds (0 . 40)))

  ("message Foo {
      enum Bar {}
    }"
   '(:message
     (:child
      (((:enum () :name "Bar" :bounds (20 . 31)))))
     :name "Foo" :bounds (0 . 37)))

  ("message Foo {
      option bar = 1;
    }"
   '(:message
     (:child
      (((:option
         ((:value . 1) (((:value () :value 1 :bounds (33 . 34)))))
         :name (("bar")) :bounds (20 . 35)))))
     :name "Foo" :bounds (0 . 41)))

  ("message Foo {
      repeated string bar = 1;
    }"
   '(:message
     (:child
      (((:field
         ((:type . 1) (((:primitive-type () :name :string :bounds (29 . 35)))))
         :name "bar"  :number 1 :label :repeated :bounds (20 . 44)))))
     :name "Foo" :bounds (0 . 50))))

(define-rule-test (extend :version 2)
  (""             nil)
  ("extend"       nil)
  ("extend Foo"   nil)
  ("extend Foo {" nil)

  ("extend Foo {}"
   '(:extend
     ((:message . 1)
      (((:type-reference () :name (:relative "Foo") :bounds (7 . 10)))))
     :bounds (0 . 13)))

  ("extend Foo/**/{/**/}"
   '(:extend
     (:field
      (((:comment () :content "" :bounds (15 . 19))))
      (:message . 1)
      (((:type-reference () :name (:relative "Foo") :bounds (7 . 10)))))
     :bounds (0 . 20)))

  ("extend Foo {
      repeated string bar = 1;
    }"
   '(:extend
     (:field
      (((:field
         ((:type . 1)
          (((:primitive-type () :name :string :bounds (28 . 34)))))
         :name "bar" :number 1 :label :repeated :bounds (19 . 43))))
      (:message . 1)
      (((:type-reference () :name (:relative "Foo") :bounds (7 . 10)))))
     :bounds (0 . 49))))

(define-rule-test (extend :version 3)
  ("extend Foo {}"                           nil)
  ("extend Foo { repeated string bar = 1; }" nil))

(define-rule-test enum
  (""                    nil)
  ("enum"                nil)
  ("enum Foo"            nil)
  ("enum Foo {"          nil)

  ("enum Foo {}"         '(:enum () :name "Foo" :bounds (0 . 11)))
  ("enum/**/ Foo/* */{}" '(:enum () :name "Foo" :bounds (0 . 19)))

  ("enum Foo {
      /* bar */
    }"
   '(:enum
     (:child (((:comment () :content " bar " :bounds (17 . 26)))))
     :name "Foo" :bounds (0 . 32)))

  ("enum Foo {
      option foo = 1;
    }"
   '(:enum
     (:child (((:option
                ((:value . 1) (((:value () :value 1 :bounds (30 . 31)))))
                :name (("foo")) :bounds (17 . 32)))))
     :name "Foo" :bounds (0 . 38))))

(define-rule-test rpc
  (""                            nil)
  ("rpc"                         nil)
  ("rpc {"                       nil)
  ("rpc foo"                     nil)
  ("rpc foo ()"                  nil)
  ("rpc foo () returns"          nil)
  ("rpc foo () returns ()"       nil)
  ("rpc foo (stream) returns ()" nil)

  ("rpc bar (Message) returns (int32) {}"
   '(:rpc
     ((:return-type . 1)
      (((:message-type
         ((:type . 1)
          (((:primitive-type () :name :int32 :bounds (27 . 32)))))
         :stream? nil :bounds (26 . 33))))
      (:argument-type . 1)
      (((:message-type
         ((:type . 1)
          (((:type-reference
             ()
             :name (:relative "Message") :bounds (9 . 16)))))
         :stream? nil :bounds (8 . 17)))))
     :name "bar" :bounds (0 . 36)))

  ("rpc bar (stream Message) returns (int32)"
   '(:rpc
     ((:return-type . 1)
      (((:message-type
         ((:type . 1)
          (((:primitive-type () :name :int32 :bounds (34 . 39)))))
         :stream? nil :bounds (33 . 40))))
      (:argument-type . 1)
      (((:message-type
         ((:type . 1)
          (((:type-reference
             ()
             :name (:relative "Message") :bounds (16 . 23)))))
         :stream? t :bounds (8 . 24)))))
     :name "bar" :bounds (0 . 40)))

  ("rpc bar (int32) returns (int32) {
      option foo = 1;
    }"
   '(:rpc
     (:option
      (((:option
         ((:value . 1) (((:value () :value 1 :bounds (53 . 54)))))
         :name (("foo")) :bounds (40 . 55))))
      (:return-type . 1)
      (((:message-type
         ((:type . 1)
          (((:primitive-type () :name :int32 :bounds (25 . 30)))))
         :stream? nil :bounds (24 . 31))))
      (:argument-type . 1)
      (((:message-type
         ((:type . 1)
          (((:primitive-type () :name :int32 :bounds (9 . 14)))))
         :stream? nil :bounds (8 . 15)))))
     :name "bar" :bounds (0 . 61))))

(define-rule-test (rpc-stream :version 2)
  (""                           nil)
  ("stream"                     nil)
  ("stream {"                   nil)
  ("stream foo"                 nil)
  ("stream foo;"                nil)
  ("stream foo ();"             nil)
  ("stream foo (int32);"        nil)
  ("stream foo (int32,int32) {" nil)

  ("stream bar (int32,int32);"
   '(:stream
     ((:type2 . 1)
      (((:primitive-type () :name :int32 :bounds (18 . 23))))
      (:type1 . 1)
      (((:primitive-type () :name :int32 :bounds (12 . 17)))))
     :name "bar" :bounds (0 . 25)))

  ("stream bar (int32,int32) {}"
   '(:stream
     ((:type2 . 1)
      (((:primitive-type () :name :int32 :bounds (18 . 23))))
      (:type1 . 1)
      (((:primitive-type () :name :int32 :bounds (12 . 17)))))
     :name "bar" :bounds (0 . 27)))

  ("stream bar (int32,int32) { option foo=1; }"
   '(:stream
     (:option
      (((:option
         ((:value . 1)
          (((:value () :value 1 :bounds (38 . 39)))))
         :name (("foo")) :bounds (27 . 40))))
      (:type2 . 1)
      (((:primitive-type () :name :int32 :bounds (18 . 23))))
      (:type1 . 1)
      (((:primitive-type () :name :int32 :bounds (12 . 17)))))
     :name "bar" :bounds (0 . 42))))

(define-rule-test (rpc-stream :version 3)
  ("stream bar (int32,int32);"                  nil)
  ("stream bar (int32,int32) {}"                nil)
  ("stream bar (int32,int32) { option foo=1; }" nil))

(define-rule-test service
  (""                                  nil)
  ("service"                           nil)
  ("service {"                         nil)
  ("service { rpc foo () returns () }" nil)

  ("service foo {}"
   '(:service () :name "foo" :bounds (0 . 14)))

  ("service/**/foo{/**/}"
   '(:service
     (:child (((:comment () :content "" :bounds (15 . 19)))))
     :name "foo" :bounds (0 . 20)))

  ("service foo {
      rpc bar (int32) returns (int32);
    }"
   '(:service
     (:child
      (((:rpc
         ((:return-type . 1)
          (((:message-type
             ((:type . 1)
              (((:primitive-type () :name :int32 :bounds (45 . 50)))))
             :stream? nil :bounds (44 . 51))))
          (:argument-type . 1)
          (((:message-type
             ((:type . 1)
              (((:primitive-type () :name :int32 :bounds (29 . 34)))))
             :stream? nil :bounds (28 . 35)))))
         :name "bar" :bounds (20 . 51)))))
     :name "foo" :bounds (0 . 58)))

  ("service foo {
      option foo = 1;
    }"
   '(:service
     (:child (((:option
                ((:value . 1) (((:value () :value 1 :bounds (33 . 34)))))
                :name (("foo")) :bounds (20 . 35)))))
     :name "foo" :bounds (0 . 41))))

(define-rule-test statement-package
  (""                  nil)
  ("package"           nil)
  ("package foo"       nil)
  ("package foo.bar"   nil)

  ("package foo;"      '(:package () :name ("foo") :bounds (0 . 12)))
  ("package foo.bar;"  '(:package () :name ("foo" "bar") :bounds (0 . 16)))
  ("package foo.bar ;" '(:package () :name ("foo" "bar") :bounds (0 . 17))))

(define-rule-test proto
  ("syntax = \"proto2\";
    syntax = \"proto2\";"
   '(:file
     (:child (((:syntax () :value 2 :bounds (0 . 18)))))
     :bounds (0 . 23))
   23
   t)
  ("message Foo {};
    syntax = \"proto2\";"
   '(:file
     (:child (((:message () :name "Foo" :bounds (0 . 14)))))
     :bounds (0 . 20))
   20
   t)

  ("/*"                   '(:file () :bounds (0 . 0)) 0)
  ("{"                    '(:file () :bounds (0 . 0)) 0)
  ("message"              '(:file () :bounds (0 . 0)) 0)

  (""                     '(:file () :bounds (0 . 0)))
  ("  "                   '(:file () :bounds (0 . 2)))
  (";"                    '(:file () :bounds (0 . 1)))
  ("//"                   '(:file
                            (:child (((:comment () :content "" :bounds (0 . 2)))))
                            :bounds (0 . 2)))
  ("package foo;"         '(:file
                            (:child (((:package () :name ("foo") :bounds (0 . 12)))))
                            :bounds (0 . 12)))
  ("import \"foo\";"      '(:file
                            (:child (((:import
                                       ()
                                       :mode nil :value "foo" :bounds (0 . 13)))))
                            :bounds (0 . 13)))
  ("syntax = \"proto2\";" '(:file
                            (:child (((:syntax () :value 2 :bounds (0 . 18)))))
                            :bounds (0 . 18)))
  ("option foo = bar;"    '(:file
                            (:child (((:option
                                       ((:value . 1) (((:value () :value "bar" :bounds (13 . 16)))))
                                       :name (("foo")) :bounds (0 . 17)))))
                            :bounds (0 . 17)))
  ("message foo {};"      '(:file
                            (:child (((:message () :name "foo" :bounds (0 . 14)))))
                            :bounds (0 . 15)))
  ("enum foo {};"         '(:file
                            (:child (((:enum () :name "foo" :bounds (0 . 11)))))
                            :bounds (0 . 12))))
