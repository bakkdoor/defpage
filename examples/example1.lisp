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
	     (:h4 (link-to test () "Test page - not within a module"))))))))


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
       (:br))))))


;; notice, that we can also define pages outside of modules, if we want to:
(defpage test () ;; will be bound to the url "/test/"
  (:html 
   (:head
    (:title "defpage : simple test page - not within a module")
    (stylesheet "layout.css"))
   (:body
    (:h2 "This page is not within a module, but simply a standalone page. :)")
    (:h3
     (link-to index (root) "Go back to start page.")))))


;; now we can start the hunchentoot webserver on port 3000
(unless (server-running)
  (start-server 3000)
  (set-debug-mode t)) ;; enable debugging
