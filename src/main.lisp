(defpackage cl-latex
  (:use :cl :util)
  (:export
   :latex-head
   :author
   :title
   :make-title
   :document
   :section
   :subsection
   :subsubsection
   :tabular
   :itemize
   :print-programs
   :example
   :make-pdf-from-latex))

(in-package :cl-latex)

(defun latex-head ()
  (format nil "\\documentclass[]{jsarticle}")) 

(defun author (name)
  "generate '\\author{name}'"
  (format nil "\\author{~a}" name))
(defun title (name)
  "generate '\\title{name}'"
  (format nil "\\title{~a}" name))
(defun make-title ()
  "generate '\\maketitle'"
  (format nil "\\maketitle"))

(defun document (&rest exprs)
  "generate \\begin{document} 'expand exprs' \\end{document}"
  (format nil "\\begin{document}~%~{~a~%~}\\end{document}"  exprs))

(defun section (&optional (name ""))
  (format nil "\\section{~a}" name))
(defun subsection (&optional (name ""))
  (format nil "\\subsection{~a}" name))
(defun subsubsection (&optional (name ""))
  (format nil "\\subsubsection{~a}" name))

(defun tabular (del list)
  "\\begin{tabular}{del} 'expand list' \\end{tabular}
the nil become \\hline"
  (format nil "\\begin{tabular}{~a}~%~{~a~%~}\\end{tabular}"
	  del
	  (mapcar
	   (lambda (line)
	     (cond (line
		    (format nil "~{~a~^ & ~} \\\\" line))
		   (t
		    (format nil "\\hline"))))
	   list)))

(defun itemize (&rest items)
  (format nil "\\begin{itemize}~%~{\\item ~a~%~}\\end{itemize}" items))

(defun print-programs (output-stream &rest programs)
  (format output-stream "~{~a~%~}" programs))

(defun make-pdf-from-latex (output-file-name &rest programs)
  (with-tempolary-directory
    (with-open-file (out (uiop:merge-pathnames* "cl-latex.tex"
						(uiop:getcwd))
			 :direction :output
			 :if-exists :supersede)
      (apply #'print-programs (cons out programs)))
    (system "ptex2pdf -l cl-latex.tex")
    (uiop:copy-file
     (uiop:merge-pathnames* "cl-latex.pdf" (uiop:getcwd))
     (uiop:merge-pathnames* output-file-name back-directory))))

(defun example (output-pdf-name)
  (make-pdf-from-latex
   output-pdf-name
   (latex-head)
   (author "endered")
   (title "test generation")
   (document
    (make-title)
    (section "this is a pen")
    (subsection "これはペンではありません")
    "これはCommon Lispだけでレポート制作を完結させたいという思惑によって作りました。"
    "楽しいですよ"
    ""
    (itemize
     "こんな感じで"
     (itemize
      "文章を"
      "列挙したり")
     "出来ます")
    ""
    "$Z_5^*の演算表$"
    ""
    (tabular "c|cccc" 
	     `((* 1 2 3 4)
	       ()
	       ,@(loop for i from 1 to 4
		       collect
		       (cons i
			     (loop for j from 1 to 4
				   collect (mod (* i j) 5))))))
    ""
    "他にも演算表をこんな感じで作れます")))
