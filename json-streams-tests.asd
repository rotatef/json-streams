(defsystem #:json-streams-tests
  :name "json-streams-tests"
  :licence "GNU General Public Licence 3.0"
  :depends-on (:json-streams :cl-quickcheck :flexi-streams)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "packages")
                         (:file "input")))))
