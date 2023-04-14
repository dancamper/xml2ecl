;;;; xml2ecl.asd

(asdf:defsystem #:xml2ecl
  :description "Examines XML data and deduces the ECL RECORD definitions necessary to parse it."
  :author "Dan S. Camper"
  :license  "Apache 2.0"
  :version "0.1.0"
  :serial t
  :depends-on (#:adopt #:flexi-streams #:fxml #:with-user-abort)
  :components ((:file "package")
               (:file "ecl_keywords")
               (:file "xml2ecl")
               (:file "userio")))
