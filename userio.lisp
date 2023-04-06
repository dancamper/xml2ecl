;;;; userio.lisp

(in-package #:xml2ecl)

;; Adopt: https://docs.stevelosh.com/adopt/usage/

(defparameter *option-version*
  (adopt:make-option 'version
                     :result-key 'version
                     :help "Display version and exit."
                     :long "version"
                     :short #\v
                     :reduce (constantly t)))

(defparameter *option-help*
  (adopt:make-option 'help
                     :result-key 'help
                     :help "Display help and exit."
                     :long "help"
                     :short #\h
                     :reduce (constantly t)))

(defparameter *option-ecl-string-type*
  (adopt:make-option 'string-type
                     :result-key 'string-type
                     :parameter "STRING-TYPE"
                     :help (format nil "ECL datatype to use for XML strings; must be one of ~
                                        UTF8|STRING|VARSTRING; defaults to UTF8")
                     :long "string-type"
                     :short #\s
                     :initial-value "UTF8"
                     :reduce #'adopt:last))

(defparameter *option-group-output*
  (adopt:make-group 'output-options
                    :title "Output Options"
                    :help "These options affect how the ECL RECORD structures are created."
                    :options (list *option-ecl-string-type*)))

(defparameter *examples*
  '(("Process a single XML data file:"
     . "xml2ecl foo.xml")
    ("Process multiple specific XML data files, using STRING datatype:"
     . "xml2ecl -s STRING foo.xml bar.xml baz.xml")
    ("Process all XML files in the current directory:"
     . "xml2ecl *.xml")
    ("Process XML data coming from a file via stdin:"
     . "cat foo.xml | xml2ecl")
    ("Process a SOAP result (with sed processing):"
     . "curl -s 'https://www.crcind.com/csp/samples/SOAP.Demo.cls?soap_method=AddInteger&Arg1=31&Arg2=11' | sed -e 's/<\?.*\?>//' | xml2ecl")))

(adopt:define-string *help-text*
  "xml2ecl examines XML data and deduces the ECL RECORD definitions necessary to parse it. ~
The resulting ECL definitions are returned via standard out, suitable for piping or pasting ~
into your favorite IDE.~@
~@
XML data can be supplied as one or more files or via standard input.~@
~@
Multiple files, if provided, are parsed as if they should have the same record structure. ~
This is useful for cases where you suspect that not all XML key/value objects are fully ~
defined in one file, and other files may contain the missing data.~@
~@
ECL keywords, in general, should not be used as field names in record definitions. ~
xml2ecl will prefix those fields with 'f_' when defining those field names.  Other ~
minor changes to the field names are also made (such as converting dashes to ~
underscores).~@
~@
The last ECL record definition in the output will be the 'root' definition; it ~
is the one you should pass to the ECL DATASET() function.  If you pass exactly ~
one file to xml2ecl then that record definition will be named after the file. ~
If you pass multiple files, or stream XML data in via standard input, then the ~
layout will be named TOPLEVEL with some added items to make it unique.~@
~@
Home: https://github.com/dancamper/xml2ecl")

(defparameter *ui*
  (adopt:make-interface :name "xml2ecl"
                        :usage "[OPTIONS] [FILE...]"
                        :summary (format nil "analyze XML data and emit ECL record ~
                                              definitions that can parse that data")
                        :help *help-text*
                        :examples *examples*
                        :contents (list
                                   *option-version*
                                   *option-help*
                                   *option-group-output*)))

;;;

(define-condition user-error (error)
  ())

(define-condition missing-file (user-error)
  ((path :initarg :path))
  (:report
   (lambda (c s)
     (format s "missing file '~A'" (slot-value c 'path)))))

;;;

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (adopt:exit 130))))

(defun run (args &key (string-type *ecl-string-type*))
  "Dev-level entry point."
  (let* ((argc (length args))
         (args (if (plusp argc) args (list *standard-input*))))
    ;; Verify that files exist
    (when (plusp argc)
      (loop for input in args
            do (unless (uiop:probe-file* input)
                 (error 'missing-file :path input))))
    (let ((*ecl-string-type* (string-upcase string-type))
          (toplevel-name (if (= argc 1)
                             (pathname-name (uiop:probe-file* (car args)))
                             (format nil "~A" (gensym "toplevel_"))))
          (result-obj nil))
      ;; Make sure the string type is recognized
      (unless (member *ecl-string-type* '("UTF8" "STRING" "VARSTRING") :test #'string=)
        (adopt:print-error-and-exit (format nil "Unknown string type '~A'" *ecl-string-type*)))
      ;; Parse files or standard input
      (loop for input in args
            do (let ((one-item (or (uiop:probe-file* input) input)))
                 (setf result-obj (process-file-or-stream one-item result-obj))))
      ;; Fixup XML child objects
      (setf result-obj (fixup result-obj))
      ;; Unwrap the top layer, which we manually iinserted
      (multiple-value-bind (new-result-obj top-xpath) (unwrap-parsed-object result-obj)
        ;; Emit ECL record definitions
        (setf *layout-names* nil)
        (format t "~A" (as-ecl-record-def new-result-obj toplevel-name))
        (format t "~A" (as-ecl-dataset-example new-result-obj toplevel-name top-xpath))))))

(defun toplevel (argv)
  "CLI-level entry point."
  #+sbcl
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui* argv)
      (cond ((gethash 'version options)
             (format t "~A~%" #.(slot-value (asdf:find-system 'xml2ecl) 'asdf:version))
             (adopt:exit))
            ((gethash 'help options)
             (adopt:print-help-and-exit *ui*)))
      (handler-case (run (cdr arguments) :string-type (gethash 'string-type options))
        (user-error (e) (adopt:print-error-and-exit e))
        (fxml:xml-parse-error (e) (adopt:print-error-and-exit e)))))
  (adopt:exit))
