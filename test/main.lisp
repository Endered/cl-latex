(ql:quickload :cl-latex)

(defpackage :main
  (:use :cl :cl-latex))

(in-package :main)


(defun main ()
  (make-pdf-from-latex
   "main.pdf"
   (latex-head)
   (author "endered")
   (title "HOGE?")
   (document
    (make-title)
    "Hello World"
    ""
    (let ((n 11))
      (tabular
       (format nil "c|~a" (make-string n :initial-element #\c))
       `(("$\\times$" ,@ (loop for i from 1 below n collect i))
	 ()
	 ,@ (loop for j from 1 below n
		  collect
		  (cons j
			(loop for i from 1 below n
			      collect (mod (* i j) n))))))))))



#-swank
(main)
