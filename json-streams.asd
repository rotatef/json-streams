(defsystem #:json-streams
  :name "json-streams"
  :licence "GNU General Public Licence 3.0"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "write-float")
               (:file "output-stream")
	       (:file "input-stream")
               (:file "parser")
               (:file "tests")))
