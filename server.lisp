(in-package :defpage.server)


(defvar *server-port* 3000)

(defun server-port ()
  *server-port*)

(defun set-server-port (port)
  (if (and port 
	   (integerp port))
      (setf *server-port* port)
      (error "port must be a integer")))


;; server variable
(defvar *server* nil)

(defun start-server (&optional (port 3000))
  (set-server-port port)
  (setf *server* (hunchentoot:start-server :port *server-port*)))


;; url dispatchers
(defclass handler ()
  ((url
    :initarg :url
    :accessor url
    :type string)
   (url-fun
    :initarg :url-fun
    :accessor url-fun
    :type function)
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))


;; hash with our handlers. 
;; key: name of the handler / name of the page
;; value: handler-objekt
(defvar *handlers* (make-hash-table))


;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))


;; exchange hunchentoots dispatcher with our own
(setf hunchentoot:*dispatch-table* (list 'our-handler))


(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))


;; returns the handler-object with a given name
(defmethod gethandler ((name symbol))
  (gethash name *handlers*))


(defmethod gethandler ((url string))
  (find url
	(gethandlers)
	:key #'url :test #'equal))


;; setf-gethandler method
;; gets called to associated a handler with a name
(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))


;; returns the url of a symbol, being the name of a page
;; defined via defpage
(defmethod url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))


(defmethod command ((name symbol) &rest args)
  (arnesi:awhen (gethandler name)
    (apply (url-fun arnesi:it) arnesi:it args)))


(defmacro with-parameters ((&rest params) &body body)
  `(let (,@(loop for p in params 
	      collecting (if (equal (symbol-name p) (symbol-name :id))
			     `(,p (s-utils:parse-integer-safely (hunchentoot:parameter (string-downcase ',p))))
			     `(,p (hunchentoot:parameter (string-downcase ',p))))))
     ,@body))