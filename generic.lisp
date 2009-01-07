(in-package :defpage.generic)


(defun empty-string (string)
  (or (null string)
      (zerop (length string))))


(defun correct-url (url)
  (and
   (not (empty-string url))
   (or
    (string-begins-with url "http://")
    (string-begins-with url "https://"))))


(defun string-begins-with (string begin-other)
  (unless (string= string begin-other)
    (> (string/= string begin-other) (- (length begin-other) 1))))

(defun join-strings (string-list seperator)
  (reduce #'(lambda (x y) (concatenate 'string x seperator y)) string-list))