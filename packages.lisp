(in-package :common-lisp-user)

(defpackage :defpage.helpers
  (:use :common-lisp)
  (:export :empty-string
	   :correct-url
	   :string-begins-with
	   :join-strings))


(defpackage :defpage
  (:use :common-lisp
	:defpage.helpers)
  (:export :handler
	   :server-port
	   :set-server-port
	   :start-server
	   :stop-server
	   :set-debug-mode
	   :gethandler
	   :url
	   :command
	   :page-url
	   :with-parameters
	   :defmodule
	   :defpage
	   :defsnippet
	   :defstyle
	   :with-page-output
	   :with-css-output
	   :link-to
	   :redirect-to
	   :stylesheet))
