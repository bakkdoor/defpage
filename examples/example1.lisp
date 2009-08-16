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


;; a basic layout/template
(deflayout std ((&key (title "defpage: example1") (stylesheets '("layout.css"))) &body content)	   
  `(:html
    (:head
     (:title ,title)
     ,@(loop for s in stylesheets collect `(stylesheet ,s)))
    (:body
     (:div :id "main"
	   ,@content))))


(defmacro submit-button (page-name module method button-value &rest params)
  "A little macro, which generates some html for a simple submit-button."
  `(with-snippet-output
     (:form :action (defpage:command ',page-name ',module ,@params) :method (cl-who:str ,method)
	    (:input :type "submit" :value ,button-value))))


;; let's define a module which will hold the homepage.
;; you could also put other top-level-pages here.  
(defmodule root ;; will be bound to root url "/".
  ;; define index page, which will automatically be bound to the root url "/".
  (defpage index ()
    (with-std-layout ()
      (:p
       (:h3 "Welcome to the first example page, created with the defpage library!"))
      (:p
       (:h4 (link-to show (other) "Link without a parameter")))
      (:p
       (:h4 (link-to show (other) "Link with a parameter" :message "Hello, world!")))
      (:p
       (:h4 (link-to test () "Test page - not within a module")))
      (:p
       (:h4 (link-to wrong-request () "Accessing this page without a post-request will fail!"))
       (:h3 "With forms:")
       (submit-button wrong-request nil :get "Access via :get method" :message "hello with :get message")
       (submit-button wrong-request nil :post "Access via :post method" :message "hello with :post message")))))


;; another module, with a page called 'show'.
;; note that, since the module's base-url is "/other" (simply the name of the module - the base-url 
;; each page within this module will also have this in front of its url.
(defmodule other
  ;; leaving the optional url parameter will automatically bind the show-page to the url "/other/show/".
  (defpage show (message) ;; page takes an optional parameter named 'message'. 
    (with-std-layout ()
      (:p
       (:h2 "Message is:")
       (:h1 (cl-who:str message))
       (:h3 (link-to index (root) "Go back to start page."))
       (:br)))))


;; notice, that we can also define pages outside of modules, if we want to:
(defpage test () ;; will be bound to the url "/test/"
  (with-std-layout (:title "defpage : simple test page - not within a module")
    (:h2 "This page is not within a module, but simply a standalone page. :)")
    (:h3
     (link-to index (root) "Go back to start page."))))

(defpage wrong-request (message)
  (ensure-post-request
   (with-std-layout (:title "This won't work, if accessed via :get method.")
     (:p
      (:h3 (cl-who:str message))
      (:h2 "If you see this, you did actually use the :post http-request-method to access this page.")
      (:h3 (link-to index (root) "Go back to start page."))))))


;; now we can start the hunchentoot webserver on port 3000
(unless (server-running)
  (start-server :port 3000)
  (set-debug-mode t)) ;; enable debugging

;; we can also define our own custom bad-request-handler,
;; if we don't like the default one (i know, it's ugly!):
(setf defpage:*bad-request-handler*
      #'(lambda ()
          (with-std-layout (:title "Bad page request.")
            (:div :style "text-align: center"
                  (:h2
                   "Bad page request. Something went wrong here.")
                  (:h4 (link-to index (root) "Go back home"))))))