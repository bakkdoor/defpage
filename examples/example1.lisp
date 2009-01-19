;; load defpage (works in sbcl) & define package
(in-package :cl-user)

;(require 'defpage)

(defpackage :defpage-example1
  (:use :common-lisp :defpage))


;; this is a little example page to show some features of defpage.

(in-package :defpage-example1)

;; actual code starts here

;; a little stylesheet for nicer layout
;; will be bound to the url "/stylesheets/layout.css"
(defstyle layout.css
  (body
   (background-color "#ccc")
   (color "#000")
   (font-size "15"))
  (a
   (color "#666")
   (text-decoration "none"))
  ("a:hover"
   (color "#611")
   (text-decoration "underline")))


;; a little snippet, that displays the module-name of a given page-nam
;; or a message, if the page is not in a module.
(defsnippet current-module (page-name &optional (module-name nil))
  (:h4 
   (cl-who:str
    (let* ((handler (defpage::gethandler module-name page-name))
	   (module (if handler
		       (defpage::module handler)
		       nil))
	   (module-name (if module
			    (defpage::module-name module)
			    nil)))
       (if module-name
	   (format nil "Current module is: ~a." module-name)
	   "Page not in a module.")))))


;; let's define a module which will hold the homepage.
;; you could also put other top-level-pages here.  
(defmodule root ;; will be bound to root url "/".
  ;; define index page, which will automatically be bound to the root url "/".
  (defpage index ()
    (:html
     (:head
      (:title "defpage : example1")
      (stylesheet "layout.css"))
     (:body
      (:div :id "main"
	    (:p
	     (:h3 "Welcome to the first example page, created with the defpage library!"))
	    (:p
	     (:h4 (link-to show (other) "Link without a parameter")))
	    (:p
	     (:h4 (link-to show (other) "Link with a parameter" :message "Hello, world!")))
	    (:p
	     (:h4 (link-to test () "Test page - not within a module")))
	    (:p
	     (current-module 'root 'index)))))))


;; another module, with a page called 'show'.
;; note that, since the module's base-url is "/other" (simply the name of the module - the base-url 
;; each page within this module will also have this in front of its url.
(defmodule other
  ;; leaving the optional url parameter will automatically bind the show-page to the url "/other/show/".
  (defpage show (message) ;; page takes an optional parameter named 'message'. 
    (:html
     (:head
      (:title "defpage - show/print a message")
      (stylesheet "layout.css"))
     (:body
      (:p
             (:h2 "Message is:")
       (:h1 (cl-who:str message))
       (:h3 (link-to index (root) "Go back to start page."))
       (:br)
       (current-module 'other 'show))))))


;; notice, that we can also define pages outside of modules, if we want to:
(defpage test () ;; will be bound to the url "/test/"
  (:html 
   (:head
    (:title "defpage : simple test page - not within a module")
    (stylesheet "layout.css"))
   (:body
    (:h2 "This page is not within a module, but simply a standalone page. :)")
    (:h3
     (link-to index (root) "Go back to start page.")
     (current-module nil 'test)))))


;; now we can start the hunchentoot webserver on port 3000
(unless defpage::*server*
  (start-server 3000)
  (set-debug-mode t)) ;; enable debugging
