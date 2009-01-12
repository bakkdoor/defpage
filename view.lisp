(in-package :defpage.view)


;; dynamic variable for cl-who outputs stream
(defvar *+html-stream+*)
(declaim (special *+html-stream+*))

;(defvar *layouts* (make-hash-table))

;; short-hand macro for cl-who:w-h-o-t-s
(defmacro with-page-output (&body body)
  "Short-hand macro for cl-who:with-html-output-to-string.
  Is used within the defpage macro to define pages."
  `(cl-who:with-html-output-to-string (*+html-stream+* nil :indent t)
     ,@body))


;; short-hand macro for using generate-css-from
;; aroung the body of the macro.
(defmacro with-css-output (&body css-definitions)
  "Short-hand macro for using generate-css-from.
  Is used within defstyle macro to define css styles."
  `(generate-css-from ',css-definitions))


;; turns given element into a string.
;; if given a list, turns each element of list into a string.
(defun stringify (elem)
  "Turns given element into a string.
  If given element is a list, turns each element of list into a string."
  (cond 
    ((null elem)
     nil)
    ((and (atom elem) elem)
     (string-downcase (string elem)))
    ((listp elem)
     (cons (stringify (car elem)) (stringify (cdr elem))))))


;; returns a css-string based upon a list of lists
;; list should be created by using with-css output syntax.
;; list is a list of lists, each list containing the css definitions for a certain css-class.
;; example:
;; (generate-css-from '(body (color "#fff") (background-color "#000")))
;; will give you something like this:
;; body{
;;   color: #fff;
;;   background-color: #000;
;; }
(defun generate-css-from (list)
  "Generates css declarations as a string based upon a list of lists which resemble the css code.
  Example: (generate-css-from '(body (color \"#fff\") (background-color \"#000\")))
  Will give you something like this:
    body{
      color: #fff;
      background-color: #000;
    }"
  (let ((css-blocks (stringify list))
	(css-string (list nil)))
    (dolist (block css-blocks)
      (setf css-string (append css-string (list (format nil "~a{~%" (car block)))))
      (dolist (item (cdr block))
	(setf css-string (append css-string (list (format nil "~t~a:~a;~%" (car item) (cadr item))))))
      (setf css-string (append css-string (list (format nil "}~%")))))
    (reduce #'(lambda (x y) (concatenate 'string x y)) css-string)))
    
      
;; macro to define a page.
;; creates & registers the appropriate handlers etc for hunchentoot
;; takes a name and a url to which the page maps
;; as well as the actual html-ouput in the body.
(defmacro defpage ((name url) (&rest args) &body body)
  "Macro to define a page.
  Creates & registers the appropriate handlers for hunchentoot.
  Takes a name and a url (as string) to which the page will be mapped.
  The body contains the html-ouput which then will be displayed to the browser (uses cl-who's html-output syntax)."
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
	   (make-instance 'handler
			  :url ,url
			  :url-fun ,(if (not args)
					`(lambda (handler)
					   (url handler))
					`(lambda (handler &key ,@args)
					   (format nil
						   ,(format nil "~~A?~{~(~A~)=~~A~^&~}"
							    args)
						   (url handler)
						   ,@args)))
			  :name ',name
			  :handler ,(if (not args)
					`(lambda ()
					   (with-page-output
					     ,@body))
					`(lambda ()
					   (with-parameters (,@args)
					     (with-page-output
					       ,@body))))))))
								    


;; define a css stylesheet
;; will be routed to url within /stylesheets/[name]
(defmacro defstyle ((name &optional (path "/stylesheets/")) &body body)
  "Defines a css stylesheet.
  Takes the name for the stylesheet (e.g. \"layout.css\" and an optional 'path' under which the stylesheet will be put.
  If no path given, the stylesheet will be mapped to the standard stylesheets-path (/stylesheets/$name)
  The body contains the css-style definitions.
  Example:
    (defstyle (layout.css)
      (body
        (color \"#fff\")))"
  (unless (null path)
    (let ((url (concatenate 'string path (string-downcase (string name)))))
      (cl-utilities:once-only (url)
	`(setf (gethandler ',name)
	       (make-instance 'defpage.server:handler
			      :url ,url
			      :name ',name
			      :handler (lambda()
					 (with-css-output
					   ,@body))))))))


;; similar to defpage, only that it doesn't define a new page
;; but just a snippet / part of a webpage.
;; similar to for example 'partials' in ruby on rails. 
(defmacro defsnippet (name args &body body)
  "Defines a new html-snippet. Basically similar to the defpage macro, 
  but instead of defining a whole new page only a snippet (or 'partial') is defined.
  Can take any amount of arguments, which can then be passed to it when called from a page defined with defpage.
  Example: (defsnippet post-title (post)
             (:div :id \"post-title\"
               (:p (cl-who:str (title post)))))"
  `(defun ,name ,args
     (cl-who:with-html-output (*+html-stream+* nil :indent t)
       ,@body)))


;;(defmacro deflayout (name args &body body)
;;  (let ((arg-list (append (list (quote &key)) args))) 
;;  `(setf (gethash ',name *layouts*) #'(lambda ,arg-list
;;					(cl-who:with-html-output (*+html-stream+*)
;;					  ,@body)))))

;;(defmacro use-layout (layout-name args)
;;  (let ((layout-fn (gensym)))  
;;    `(let* ((,layout-fn (gethash ',layout-name *layouts*)))
;;       (cl-who:with-html-output (*+html-stream+*)
;;	 (funcall ,layout-fn ,@args)))))


;; redirects to a given page name.
;; example: (redirect-to home-page)
(defmacro redirect-to (page-name &optional &rest page-arguments)
  "Lets hunchentoot redirect to a given page-name with optionally any amount of page-arguments.
  Example: (redirect-to home-page)
  (where home-page would be defined as a page via defpage)." 
  (if page-arguments
      `(hunchentoot:redirect (command ',page-name ,@page-arguments))
      `(hunchentoot:redirect (url ',page-name))))
    
;; takes the page-name (name defined within a defpage definition)
;; and an optional title (text displayed for the link) or takes
;; the page-name as the title, if title not given and returns the
;; html for a link to the page.
(defmacro link-to (page-name &optional (title nil title-given-p) &rest page-arguments)
  "Creates a link to a given page-name (defined with defpage) with an optional title and any amount of page arguments."
  (let ((link-title (if (and title-given-p title)
			title
			(string-capitalize page-name))))
    `(cl-who:with-html-output (*+html-stream+*)
       (cl-who:htm
	,(if page-arguments
	     `(:a :href (command ',page-name ,@page-arguments)
		  ,link-title)
	     `(:a :href (url ',page-name)
		  ,link-title))))))
		    

;; helper-snippet to link to a stylesheet file
(defsnippet stylesheet (name &optional (path "/stylesheets/"))
  "Creates a stylesheet-html-tag to a stylesheet defined with the given name."
  (:link :href (format nil "~a~a" path name) :rel "stylesheet" :type "text/css"))