(in-package :defpage.generic)


(defun empty-string (string)
  "Indicates, if a given string is empty (or being nil)."
  (or (null string)
      (zerop (length string))))


(defun string-begins-with (string begin-string)
  "Indicates, if a given string begins with a another given string. The first string actually needs to start with the second and not be the same for this function to return anything but nil."
  (if (or (string= string begin-string) 
	   (empty-string string)
	   (empty-string begin-string))
      nil
      (> (string/= string begin-string) (- (length begin-string) 1))))


(defun correct-url (url)
  "Indicates, if a given url (as a string) is a correct & valid url"
  (and
   (not (empty-string url))
   (or
    (string-begins-with url "http://")
    (string-begins-with url "https://"))))


(defun join-strings (string-list seperator)
  "Joins the given list of strings with the given seperator.
  For example: (join-strings '(\"hello\" \"world\") \",\") 
                --> 'hello, world'"
  (reduce #'(lambda (x y) (concatenate 'string x seperator y)) string-list))