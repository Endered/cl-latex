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
   :make-pdf-from-latex
   :make-formula
   :need-package
   :$))

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
(defun need-package (package &optional (option nil))
  (format nil "\\usepackage~a{~{~a~^,~}}"
	  (if option (format nil "[~a]" option) "")
	  (castlist package)))

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
    (multiple-value-bind
	  (result tmp code)
	(system "ptex2pdf -l cl-latex.tex")
      (cond ((not (eq code 0))
	     (format t "~a~%" result))
	    (t
	     (uiop:copy-file
	      (uiop:merge-pathnames* "cl-latex.pdf" (uiop:getcwd))
	      (uiop:merge-pathnames* output-file-name back-directory)))))))

(defun renewcommand (origin next)
  (format nil "\\renewcommand{~a}{~a}" origin next))

(defun $ (expr)
  (format nil "$~a$" (make-formula expr)))

(defun make-formula (expr)
  (labels ((bra (form priority)
	     (if (> (cdr form) priority)
		 (format nil "(~a)" (car form))
		 (car form)))
	   (bras (lst priority)
	     (mapcar (lambda (form) (bra form priority)) lst))
	   (eq-symbol (a b)
	     (string= (symbol-name a) (symbol-name b)))
	   (eq-symbols (lst b)
	     (some (lambda (a) (eq-symbol a b)) lst))
	   (rec (expr)
	     (cond ((symbolp expr)
		    (cons (string-downcase (princ-to-string expr))
			  0))
		   ((atom expr)
		    (cons (format nil "~a" expr) 0))
		   ((eq-symbol '+ (car expr))
		    (cons (format nil "~{~a~^ + ~}"
				  (bras (mapcar #'rec (cdr expr))
					2))
			  2))
		   ((eq-symbol '- (car expr))
		    (cons (format nil "~{~a~^ - ~}"
				  (bras (mapcar #'rec (cdr expr))
					2))
			  2))
		   ((eq-symbol '* (car expr))
		    (cons (format nil "~{~a~^ \\times ~}"
				  (bras (mapcar #'rec (cdr expr))
					1))
			  1))
		   ((eq-symbol '/ (car expr))
		    (cons (if (eq 2 (length expr))
			      (format nil "\\frac{1}{~a}"
				      (bra (rec (cadr expr)) 2))
			      (format nil "\\frac{~a}{~a}"
				      (bra (rec (cadr expr)) 2)
				      (bra (rec (cons '* (cddr expr))) 2)))
			  1))
		   ((eq-symbols '(_ under) (car expr))
		    (cons (format nil "~a_{~a}"
				  (bra (rec (cadr expr)) 0)
				  (bra (rec (caddr expr)) 100))
			  0))
		   ((eq-symbols '(^ expt) (car expr))
		    (cons (format nil "~a^{~a}"
				  (bra (rec (cadr expr)) 0)
				  (bra (rec (caddr expr)) 100))
			  0))
		   ((eq-symbol 'sqrt (car expr))
		    (cons (format nil "\\sqrt{~a}"
				  (bra (rec (cadr expr)) 100))
			  0))
		   ((eq-symbol 'set (car expr))
		    (cons (if (eq 2 (length expr))
			      (format nil "\\left\\{~a\\right\\}"
				      (bra (rec (cadr expr)) 100))
			      (format nil "\\left\\{~a|~{~a~^,~}\\right\\}"
				      (bra (rec (cadr expr)) 100)
				      (bras (mapcar #'rec (cddr expr))
					    100)))
			  0))
		   ((eq-symbols '(eq =) (car expr))
		    (cons (format nil "~{~a~^ = ~}"
				  (bras (mapcar #'rec (cdr expr))
					100))
			  0))
		   ((eq-symbol 'funcall (car expr))
		    (cons (format nil "~a\\left(~{~a~^, ~}\\right)"
				  (bra (rec (cadr expr)) 0)
				  (bras (mapcar #'rec (cddr expr))
					100))
			  0))
		   ((eq-symbol 'command (car expr))
		    (cons (format nil "~a~{{~a}~}"
				  (bra (rec (cadr expr)) 100)
				  (bras (mapcar #'rec (cddr expr))
					100))
			  0))
		   ((eq-symbol 'cat (car expr))
		    (cons (format nil "~{~a~^ ~}"
				  (bras (mapcar #'rec (cdr expr))
					100))
			  0))
		   (t
		    (error "There are no symbol")))))
    (car (rec expr))))

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
