(in-package :defpage)


;; dynamic variable for cl-who outputs stream
(defvar *+html-stream+*)
(declaim (special *+html-stream+*))

;; dynamic variable for current module
(defvar *+module+* nil)
(declaim (special *+module+*))

(defclass module ()
  ((name
    :reader module-name
    :initarg :name
    :type symbol)
   (url
    :reader module-url
    :initarg :url
    :type string)))


(defmacro with-page-output (&body body)
  "Short-hand macro for cl-who:with-html-output-to-string.
  Is used within the defpage macro to define pages."
  `(cl-who:with-html-output-to-string (*+html-stream+* nil :indent t)
     ,@body))


(defmacro with-css-output (&body css-definitions)
  "Short-hand macro for using generate-css-from.
  Is used within defstyle macro to define css styles."
  `(generate-css-from ',css-definitions))


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


(defmacro defmodule (name &body body)
  "Defines a new module, into which pages can be defined.
  Creates a kind of 'folder' into which pages will be put."
  (let ((base-url (concatenate 'string "/" (string-downcase name))))
    (if (string= base-url "/root")
	(setf base-url "/"))
    `(let ((*+module+* (make-instance 'module :name ',name :url ,base-url)))
       ,@body)))      
 

(defmacro defpage (name (&rest args) &body body)
  "Macro to define a page.
  Creates & registers the appropriate handlers for hunchentoot.
  Takes a name and maps it to a url consisting of the name to which the page will be mapped.
  Also takes a list of possible arguments to the page.
  The body contains the html-ouput which then will be displayed to the browser (uses cl-who's html-output syntax).
  Pages named \"root\" or \"index\" will be mapped to this url: \"/\"."
  (let ((url (concatenate 'string "/" (string-downcase name) "/")))
    (if (or (string= url "/root/")
	    (string= url "/index/"))
	(setf url "/"))
    (let ((url-var (gensym)) ;; this var will hold the actual url value
	  (curr-module-name (gensym)))
      (cl-utilities:once-only (url)
	`(progn
	   (let* ((,url-var ,url)
		  (,curr-module-name (if *+module+*
					 (module-name *+module+*)
					 nil)))
	     ;; if inside a module, add the module-url in front of the handler's
	     (if (and *+module+* 
		      (string/= (module-url *+module+*) "/"))
		 (setf ,url-var (concatenate 'string (module-url *+module+*) ,url)))
 	     (sethandler ',name (,curr-module-name)
			 (make-instance 'handler
					:module *+module+*
					:url ,url-var
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
							       ,@body))))))))))))


(defmacro defstyle (name &body body)
  "Defines a css stylesheet.
  Takes the name for the stylesheet (e.g. \"layout.css\".
  The stylesheet will be mapped to the standard stylesheets-path, together with its name (/stylesheets/$name).
  The body contains the css-style definitions.
  Example:
    (defstyle layout.css
      (body
        (color \"#fff\")))"
  (let ((path "/stylesheets/"))
    (let ((url (concatenate 'string path (string-downcase (string name)))))
      (cl-utilities:once-only (url)
	`(setf (gethandler ',name)
	       (make-instance 'handler
			      :url ,url
			      :name ',name
			      :handler (lambda()
					 (with-css-output
					   ,@body))))))))


(defmacro with-snippet-output (&body body)
  `(cl-who:with-html-output (defpage::*+html-stream+* nil :indent t)
     ,@body))



(defmacro defsnippet (name args &body body)
  "Defines a new html-snippet. Basically similar to the defpage macro, 
  but instead of defining a whole new page only a snippet (or 'partial') is defined.
  Can take any amount of arguments, which can then be passed to it when called from a page defined with defpage.
  Example: (defsnippet post-title (post)
             (:div :id \"post-title\"
               (:p (cl-who:str (title post)))))"
  `(defun ,name ,args
     (cl-who:with-html-output (defpage::*+html-stream+* nil :indent t)
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



(defmacro redirect-to (page-name &optional (module-name nil) &rest page-arguments)
  "Lets hunchentoot redirect to a given page-name with optionally any amount of page-arguments.
  Example: (redirect-to home-page)
  (where home-page would be defined as a page via defpage)." 
  (if page-arguments
      `(hunchentoot:redirect (command ',page-name ',module-name ,@page-arguments))
      `(hunchentoot:redirect (page-url ',page-name ',module-name))))

;; takes the page-name (name defined within a defpage definition)
;; and an optional title (text displayed for the link) or takes
;; the page-name as the title, if title not given and returns the
;; html for a link to the page.
(defmacro link-to (page-name (&optional (module nil)) &optional (title nil title-given-p) &rest page-arguments)
  "Creates a link to a given page-name (defined with defpage) with an optional title and any amount of page arguments."
  (let ((link-title (if (and title-given-p title)
			title
			(string-capitalize page-name))))
    `(cl-who:with-html-output (*+html-stream+*)
       (cl-who:htm
	,(if page-arguments
	     `(:a :href (command ',page-name ',module ,@page-arguments)
		  ,link-title)
	     `(:a :href (page-url ',page-name ',module)
		  ,link-title))))))


;; helper-snippet to link to a stylesheet file
;; Creates a stylesheet-html-tag to a stylesheet defined with the given name.
(defsnippet stylesheet (name &optional (path "/stylesheets/"))
  (:link :href (format nil "~a~a" path name) :rel "stylesheet" :type "text/css"))