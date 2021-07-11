(defsystem :cl-latex
  :author "endered"
  :license ""
  :depends-on ("trivial-shell")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("util"))
		 (:file "util")))))
