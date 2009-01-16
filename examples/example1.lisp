;; this is a little example page to show some features of defpage.

(in-package :defpage-example1)

;; actual code starts here

;; a little stylesheet for nicer layout
(defstyle (layout.css)
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

;; let's define a controller which will hold the homepage.
;; you could also put other top-level-pages here.  
(defcontroller (main "/")
  ;; define page, home, and bind it to the root url "/".
  (defpage (home "/") ()
    (:html
     (:head
      (:title "defpage : example1")
      (stylesheet "layout.css"))
     (:body
      (:div :id "main"
	    (:p
	     (:h3 "Welcome to the first example page, created with the defpage library!"))
	    (:p
	     (:h4 (link-to show "Link without a parameter")))
	    (:p
	     (:h4 (link-to show "Link with a parameter" :message "Hello, world!")))
	    (:p
	     (:h4 (link-to test "Test page - not within a controller"))))))))

;; another controller, with a page called 'show'.
;; note that, since the controller's base-url is "/other" (simply the name of the controller - the base-url 
;; can also be passed in as the second (optional) parameter after the name of the controller), 
;; each page within this controller will also have this in front of its url.
(defcontroller (other)
  ;; leaving the optional url parameter will automatically bind the show-page to the url "/other/show/".
  (defpage (show) (message) ;; page takes an optional parameter named 'message'. 
    (:html
     (:head
      (:title "defpage - show/print a message")
      (stylesheet "layout.css"))
     (:body
      (:p
       (:h2 "Message is:")
       (:h1 (cl-who:str message))
       (:h3 (link-to home "Go back to start page.")))))))


;; notice, that we can also define pages outside of controllers, if we want to:
(defpage (test "/test") ()
  (:html 
   (:head
    (:title "defpage : simple test page - not within a controller")
    (stylesheet "layout.css"))
   (:body
    (:h2 "This page is not within a controller, but simply a standalone page. :)")
    (:h3
     (link-to home "Go back to start page.")))))


;; now we can start the hunchentoot webserver on port 3000
(unless defpage::*server*
  (start-server 3000)
  (set-debug-mode t)) ;; enable debugging
