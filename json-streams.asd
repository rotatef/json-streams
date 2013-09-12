(defsystem #:json-streams
  :name "json-streams"
  :licence "GNU General Public Licence 3.0"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "write-float")
               (:file "base")
               (:file "output-stream")
	       (:file "input-stream")
               (:file "parse")
               (:file "stringify")
               (:file "tests")))
