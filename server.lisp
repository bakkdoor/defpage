(in-package :defpage)


(defvar *server-port* 3000
  "The port on which the hunchentoot server is listening on.")

(defvar *bad-request-handler* nil
  "The handler function that gets called, if a bad request occured.")

(defun server-port ()
  "Returns the the port on which hunchentoot listens on."
  *server-port*)

(defun set-server-port (port)
  "Sets the port on which hunchentoot will listen."
  (if (and port 
	   (integerp port))
      (setf *server-port* port)
      (error "port must be a integer")))


;; server variable
(defvar *server* nil
  "The server variable, holding the current running server instance of hunchentoot.")

(defvar *acceptor* nil
  "The acceptor variable")

(defun start-server (&key (port 3000) (mod-lisp-p nil))
  "Starts the hunchentoot server on a given port."
  (set-server-port port)
  (setf *acceptor* (make-instance 'hunchentoot:acceptor :port *server-port* ));:mod-lisp-p mod-lisp-p))
  (setf *server* (hunchentoot:start *acceptor*)))
  ;(setf *server* (hunchentoot:start-server :port *server-port* :mod-lisp-p mod-lisp-p)))


(defun stop-server ()
  "Stops the hunchentoot server"
  ;(hunchentoot:stop-server *server*)
  (hunchentoot:stop *acceptor*)
  (setf *server* nil))


(defun server-running ()
  "Returns T or NIL, indicating if the hunchentoot server is running or not."
  (not (null *server*)))

(defun set-debug-mode (value)
  "Takes a boolean value and turns the debugging information display on or off, depending on value."
  (setf hunchentoot:*show-lisp-errors-p* value))
;  (setf hunchentoot:*show-lisp-backtraces-p* value))

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
   (module
    :initarg :module
    :accessor module
    :type module)
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
(defvar *handlers* (make-hash-table :test #'equal))


;; now, we define our new dispatcher that uses the new data structure
(defun our-handler (request)
  (let ((handler (gethandler (hunchentoot:script-name request))))
    (when handler (handler handler))))


;; exchange hunchentoots dispatcher with our own
(setf hunchentoot:*dispatch-table* (list 'our-handler))


(defun gethandlers ()
  (arnesi:hash-table-values *handlers*))


(defun module-page-name (module-name page-name)
  (if module-name
      (concatenate 'string (string module-name) "--" (string page-name))
      (concatenate 'string "--" (string page-name))))


(defmacro sethandler (page-name (&optional (module-name nil)) value)
  `(setf (gethandler ,page-name ,module-name)
	 ,value))


(defgeneric gethandler (page-name &optional module-name))


;; returns the handler-object with a given name
(defmethod gethandler ((page-name symbol) &optional (module-name nil))
  (let ((name (module-page-name module-name page-name)))
    (gethash name *handlers*)))


(defmethod gethandler ((url string) &optional (module-name nil))
  (if module-name
      (setf url (concatenate 'string "/" (string-downcase module-name) url)))
  (find url
	(gethandlers)
	:key #'url :test #'equal))


;; setf-gethandler method
;; gets called to associate a handler with a name
(defun (setf gethandler) (handler page-name &optional (module-name nil))
  (let ((name (module-page-name module-name page-name)))
    (setf (gethash name *handlers*) handler)))


;; returns the url of a symbol, being the name of a page
;; defined via defpage
(defmethod page-url ((page-name symbol) &optional (module-name nil))
  (arnesi:awhen (gethandler page-name module-name)
      (url arnesi:it)))


(defmethod command ((page-name symbol) &optional (module-name nil) &rest args)
  (arnesi:awhen (gethandler page-name module-name)
    (apply (url-fun arnesi:it) arnesi:it args)))


(defmacro with-parameters ((&rest params) &body body)
  `(let (,@(loop for p in params 
	      collecting (if (equal (symbol-name p) (symbol-name :id))
			     `(,p (s-utils:parse-integer-safely (hunchentoot:parameter (string-downcase ',p))))
			     `(,p (hunchentoot:parameter (string-downcase ',p))))))
     ,@body))