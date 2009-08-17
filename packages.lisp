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
	   :*bad-request-handler*
	   :bad-request
	   :server-port
	   :set-server-port
	   :start-server
	   :stop-server
	   :server-running
	   :set-debug-mode
	   :gethandler
	   :url
	   :command
	   :page-url
	   :with-parameters
	   :defmodule
	   :deflayout
	   :defpage
	   :defsnippet
	   :defstyle
           :use-template
	   :with-page-output
	   :with-snippet-output
	   :with-css-output
	   :link-to
	   :redirect-to
	   :stylesheet
           :ensure-request
           :ensure-request-or
           :ensure-post-request
           :ensure-get-request))
