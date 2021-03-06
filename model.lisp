(in-package :defpage)


(defvar *model-db-path* "./"
  "Path to the model's database (either actual filesystem path or host/user/password connection
   string to a database server")

(defvar *elephant-store*
  `(elephant:open-store (:clsql (:sqlite3 ,(concatenate 'string *model-db-path* "defpage-model.db")))))

(defun set-db-store (&key (db-type :sqlite3) (path nil))
  (unless path
    (setf path (concatenate 'string *model-db-path* "defpage-model.db")))
  (setf *elephant-store*
        (elephant:open-store
         `(:clsql (,db-type ,path)))))


(defmacro defmodel (model-name (&optional (model-plural-name nil)) slot-definitions)
  (let* ((model-plural-name (if model-plural-name
                                (string-downcase model-plural-name)
                                (concatenate 'string (string-downcase model-name) "s")))
         (collection-name (intern (concatenate 'string "*" (string-upcase model-plural-name) "*")))
         (get-method-name (intern (concatenate 'string "GET-" (string-upcase model-name) "S"))))
    `(progn
       (elephant:defpclass ,model-name ()
         ,(mapcar (lambda (sd)
                    (let* ((slot-name (first sd))
                           (slot-modifiers (rest sd)))
                      `(,slot-name  :initarg ,(intern (string-upcase `,slot-name) "KEYWORD")
                                    :accessor ,slot-name
                                    ,@slot-modifiers)))
                  slot-definitions))
       (defvar ,collection-name
         (or (elephant:get-from-root ,model-plural-name)
             (let ((data (elephant:make-pset)))
               (elephant:add-to-root ,model-plural-name data)
               data)))
       (defun ,get-method-name ()
         (elephant:pset-list ,collection-name))
       (create-add-method ,model-name ,collection-name ,slot-definitions))))


(defmacro create-add-method (model-name collection-name slot-definitions)
  (let* ((method-name (intern (concatenate 'string "ADD-" (string-upcase model-name))))
         (slotnames (loop for s in slot-definitions collect (first s)))
         (slot-keyword-value-pairs (loop for s in slotnames collect `(,(intern (string-upcase s) "KEYWORD") ,s)))
         (slot-keyword-value-list (reduce (lambda (x y) (append y x)) slot-keyword-value-pairs)))
    `(defun ,method-name (&key ,@(loop for s in slotnames collect `(,s NIL)))
       (elephant:insert-item
        (make-instance ',model-name ,@slot-keyword-value-list)
        ,collection-name))))