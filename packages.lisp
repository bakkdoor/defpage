(in-package :common-lisp-user)


(defpackage :defpage.server
  (:use :common-lisp)
  (:export :handler
	   :*handlers*
	   :server-port
	   :set-server-port
	   :start-server
	   :gethandler
	   :page-url
	   :with-parameters))


(defpackage :defpage.view
  (:use :common-lisp
	:defpage.server)
  (:export ;:*layouts*
	   :defpage
	   :defsnippet
	   ;:deflayout
	   :defstyle
	   :with-page-output
	   :with-css-output
	   :link-to
	   :redirect-to
	   :stylesheet))


(defpackage :defpage.generic
  (:use :common-lisp)
  (:export :empty-string
	   :correct-url
	   :string-begins-with))


(defpackage :defpage
  (:use :common-lisp
	:defpage.server
	:defpage.view
	:defpage.generic))
