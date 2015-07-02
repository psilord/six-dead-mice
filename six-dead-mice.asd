(defpackage #:six-dead-mice-asd
  (:use :cl :asdf))

(in-package #:six-dead-mice-asd)

(defsystem #:six-dead-mice
  :description "Experimental compiler for language: six-dead-mice"
  :version "0"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :licence "Apache License, Version 2.0"
  :serial t

  :depends-on ()

  :components ( (:file "package")
                (:file "lisp-object")
                (:file "characters-dictionary")
                (:file "reader-dictionary")
                (:file "six-dead-mice")))
