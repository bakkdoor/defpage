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
   (handler
    :initarg :handler
    :accessor handler)
   (name
    :initarg :name
    :accessor name
    :type symbol)))


;; hash mit unseren handlern. 
;; key: name des handlers bzw. der seite
;; value: handler-objekt
(defvar *handlers* (make-hash-table))


;; hunchentoot-handler definieren
(defun our-handler (request)
  (let ((handler (find (hunchentoot:script-name request)
		       (arnesi:hash-table-values *handlers*)
		       :key #'url :test #'equal))) 
    (when handler (handler handler))))


;; hunchentoots dispatcher mit eigenem ersetzen
(setf hunchentoot:*dispatch-table* (list 'our-handler))


;; returns the handler-object with a given name
(defun gethandler (name)
  (gethash name *handlers*))


;; setf-gethandler method
;; gets called to associated a handler with a name
(defun (setf gethandler) (handler name)
  (setf (gethash name *handlers*) handler))


;; returns the url of a symbol, being the name of a page
;; defined via defpage
(defmethod page-url ((name symbol))
  (arnesi:awhen (gethandler name)
    (url arnesi:it)))


(defmacro with-parameters ((&rest params) &body body)
  `(let (,@(loop for p in params collecting `(,p (hunchentoot:parameter (string-downcase ',p)))))
     ,@body))