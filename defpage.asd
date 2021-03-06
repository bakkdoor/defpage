(defpackage :defpage-system (:use :asdf :cl))
(in-package :defpage-system)

(defsystem defpage
  :serial t
  :name "defpage"
  :author "Christopher Bertels, Eric Normand"
  :version "0.2"
  :maintainer "Christopher Bertels"
  :licence "BSD style"
  :description "Web development with Common Lisp and hunchentoot."
  :long-description "defpage aims to be a small but useful web-development library (or even framework?) for Common Lisp  using the Hunchentoot webserver."
  :depends-on (:hunchentoot :cl-who :s-utils :arnesi :cl-utilities :html-template :elephant)
  :components ((:file "packages")
               (:file "server")
               (:file "helpers")
	       (:file "view")
               (:file "model")))
