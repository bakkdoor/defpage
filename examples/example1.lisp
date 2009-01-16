;; this is a little example page to show some features of defpage.

(in-package :defpage-example1)

;; actual code starts here

;; a little stylesheet for nicer layout
(defstyle (layout.css)
  (body
   (background-color "#ccc")
   (color "#000")
   (font-size "14"))
  (a
   (text-decoration "none"))
  ("a:hover"
   (text-decoration "underline")))

(defpage (show "/show") (message)
  (:html
   (:head
    (:title "defpage - show/print a message")
    (stylesheet "layout.css"))
   (:body
    (:p
     (:h2 "Message is:")
     (:h1 (cl-who:str message))))))

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
	   (:h4 (link-to show "Link with a parameter" :message "Hello, world!")))))))



;; now we can start the server

(start-server 3000) ;; start hunchentoot webserver on port 3000
(set-debug-mode t) ;; enable debugging
