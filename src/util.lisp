(defpackage :util
  (:use :cl)
  (:export
   :with-tempolary-directory
   :system
   :back-directory
   :with-move-directory))

(in-package :util)


(defmacro with-move-directory (directory &body body)
  (let ((back-directory (gensym)))
    `(let* ((,back-directory (uiop:getcwd))
	    (back-directory ,back-directory))
       (unwind-protect
	    (progn (uiop:chdir ,directory)
		   ,@body)
	 (uiop:chdir ,back-directory)))))

(defmacro with-tempolary-directory (&body body)
  (let ((back-directory (gensym))
	(tmp-directory (gensym)))
    `(let ((,tmp-directory
	     (loop for ,tmp-directory =
				      (format nil "/tmp/cl-latex-tmp~a/"
					      (random (expt 10 30)))
		   while (or (uiop:directory-exists-p ,tmp-directory)
			     (uiop:file-exists-p ,tmp-directory))
		   finally (return ,tmp-directory))))
       (uiop:ensure-all-directories-exist (list ,tmp-directory))
       (with-move-directory ,tmp-directory ,@body)
       (uiop:delete-directory-tree (uiop:pathname-directory-pathname ,tmp-directory) :validate t))))

(defun system (cmd)
  (trivial-shell:shell-command cmd))
