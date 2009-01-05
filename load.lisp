;; you can use this file to load & compile all files if you don't want to use asdf for doing this.
;; you probably should use asdf though, since it is easier to handle, i guess.


(in-package :cl-user)

(require 'hunchentoot)
(require 'cl-who)
(require 's-utils)
(require 'arnesi)
(require 'cl-utilities)


(defvar *current-dir*
  "/Users/bakkdoor/projekte/lisp/defpage/")

;; compiles and loads a lisp source file.
(defun compile-and-load (filename &optional &key (dir nil))
  (let ((current-dir *current-dir*))
    (if dir
	(setf current-dir dir))
    (let ((filepath (concatenate 'string current-dir filename)))
      (compile-file filepath)
      (load filepath))))

 
;; takes ny list of symbols and returns them as a list,
;; each converted as a string 
(defmacro as-string-list (&rest filenames)
  `(list ,@(loop for f in filenames collecting `,(string-downcase f))))


;; define files to load & compile
(defvar *files-to-load-and-compile*
  (as-string-list packages 
		  server 
		  generic 
		  view))


;; load & compile all relevant files.
(dolist (file *files-to-load-and-compile*)
  (compile-and-load file))