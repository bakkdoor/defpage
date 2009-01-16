;; load defpage (works in sbcl) & define package
(in-package :cl-user)

;(require 'defpage)

(defpackage :defpage-example1
  (:use :common-lisp :defpage))
