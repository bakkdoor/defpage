(in-package :defpage.view)


;; dynamic variable for cl-who outputs stream
(defvar *+html-stream+*)
(declaim (special *+html-stream+*))

;(defvar *layouts* (make-hash-table))

;; short-hand macro for cl-who:w-h-o-t-s
(defmacro with-page-output (&body body)
  `(cl-who:with-html-output-to-string (*+html-stream+* nil :indent t)
     ,@body))


;; short-hand macro for using generate-css-from
;; aroung the body of the macro.
(defmacro with-css-output (&body css-definitions)
  `(generate-css-from ',css-definitions))


;; turns given element into a string.
;; if given a list, turns each element of list into a string.
(defun stringify (elem)
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
  (cl-utilities:once-only (url)
    `(setf (gethandler ',name)
	   (make-instance 'handler
			  :url ,url
			  :url-fun ,(if (not args)
					`(lambda (handler)
					   (url handler))
					`(lambda (handler &optional &key ,@args)
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
  (if page-arguments
      `(hunchentoot:redirect (command ',page-name ,@page-arguments))
      `(hunchentoot:redirect (url ',page-name))))
    
;; takes the page-name (name defined within a defpage definition)
;; and an optional title (text displayed for the link) or takes
;; the page-name as the title, if title not given and returns the
;; html for a link to the page.
(defmacro link-to (page-name &optional (title nil title-given-p) &rest page-arguments)
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
  (:link :href (format nil "~a~a" path name) :rel "stylesheet" :type "text/css"))