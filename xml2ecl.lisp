;;;; xml2ecl.lisp

(in-package #:xml2ecl)

; (declaim (optimize (debug 3)))

;;;

(defvar *layout-names* nil
  "Used while ECL record definitions are being emitted.  Tracks the names
of the record definitions created, so that subsequent creations don't reuse
previously-defined names.")

(defparameter *ecl-string-type* "UTF8"
  "The ECL data type to be used for XML string types.  Can be overridden
with an option.")

(defparameter *inner-text-tag* "_inner_value"
  "The ECL field name to use for value found within an XML tag when that
tag has attributes, or in cases where a simple tag is repeated within
an XML scope.")

;;;

(defclass object-item ()
  ((children :accessor children :initform (make-hash-table :test 'equalp :size 25))
   (attrs :accessor attrs :initform (make-hash-table :test 'equalp :size 10))
   (visit-count :accessor visit-count :initform 0)
   (max-visit-count :accessor max-visit-count :initform 0)))

(defmethod reset-visits ((obj t))
  )

(defmethod reset-visits ((obj object-item))
  (setf (max-visit-count obj) (max (visit-count obj) (max-visit-count obj))
        (visit-count obj) 0))

(defmethod reset-children-visits ((obj object-item))
  (loop for child being the hash-values of (children obj)
        do (reset-visits child)))

(defmethod only-inner-text-p ((obj t))
  nil)

(defmethod only-inner-text-p ((obj object-item))
  (and (zerop (hash-table-count (attrs obj)))
       (= (hash-table-count (children obj)) 1)
       (gethash *inner-text-tag* (children obj))))

(defmethod single-inner-text-p ((obj t))
  nil)

(defmethod single-inner-text-p ((obj object-item))
  (and (= (max-visit-count obj) 1)
       (only-inner-text-p obj)))

;;;

(defun is-ecl-keyword-p (name)
  "Test if NAME (which should be a lowercase string) is an ECL keyword."
  (member name *ecl-keywords* :test 'equalp))

(defun remove-illegal-chars (name &key (replacement-char #\_) (keep-char-list '()))
  "Return a copy of NAME with characters illegal for ECL attribute names
substituted with a replacment character, then reducing runs of those
replacement characters down to a single occurrence."
  (let* ((keep-chars (reduce 'cons keep-char-list
                             :initial-value (list #\_ replacement-char)
                             :from-end t))
         (initial (substitute-if replacement-char
                                 (lambda (c) (not (or (alphanumericp c) (member c keep-chars))))
                                 name))
         (skip nil)
         (result (with-output-to-string (s)
                   (loop for c across initial
                         do (progn
                              (unless (and (eql c replacement-char) skip)
                                (format s "~A" c))
                              (setf skip (eql c replacement-char)))))))
    result))

;;;

(defun apply-prefix (name prefix-char)
  (format nil "~A~A~A"
          prefix-char
          (if (char= (elt name 0) #\_) "" "_")
          name))

(defun legal-layout-subname (name)
  "Return a copy of NAME that can be used within a RECORD name."
  (let ((initial (string-upcase (remove-illegal-chars name))))
    (if (not (alpha-char-p (elt initial 0)))
        (apply-prefix initial "F")
        initial)))

(defun register-layout-subname (name)
  "Push layout subname NAME to a special variable list so we can track usage."
  (let ((legal-name (legal-layout-subname name)))
    (push legal-name *layout-names*)))

;;;

(defun as-layout-name (name)
  "Construct a string that is a suitable ECL RECORD attribute, based on NAME."
  (let* ((legal-name (legal-layout-subname name))
         (name-count (count-if #'(lambda (x) (equalp x legal-name)) *layout-names*))
         (interstitial (if (< name-count 2) "" (format nil "_~3,'0D" name-count))))
    (format nil "~A~A_LAYOUT" legal-name interstitial)))

(defun as-ecl-field-name (name)
  "Return a copy of NAME that is suitable to be used as an ECL attribute."
  (let* ((lowername (string-downcase name))
         (no-dashes (remove-illegal-chars lowername))
         (legal (if (or (not (alpha-char-p (elt no-dashes 0)))
                        (is-ecl-keyword-p no-dashes))
                    (apply-prefix no-dashes "f")
                    no-dashes)))
    (if (string= lowername *inner-text-tag*)
        lowername
        legal)))

(defun as-ecl-xpath (name attributep)
  "Construct an ECL XPATH directive for NAME (typically an as-is XML tag)."
  (if (string= name *inner-text-tag*)
      "{XPATH('')}"
      (let ((cleaned-name (remove-illegal-chars name :replacement-char #\* :keep-char-list '(#\-)))
            (attr-prefix (if attributep "@" "")))
        (format nil "{XPATH('~A~A')}" attr-prefix cleaned-name))))

(defun as-dataset-type (name)
  "Construct an ECL DATASET datatype, given NAME."
  (format nil "DATASET(~A)" (as-layout-name name)))

(defun as-ecl-type (value-type)
  "Given a symbol representing an internal data type, return the corresponding ECL data type."
  (if (consp value-type)
      (as-ecl-type (reduce-base-type value-type))
      (case value-type
        (boolean "BOOLEAN")
        (null-value "STRING")
        (string "STRING")
        (default-string *ecl-string-type*)
        (pos-number "UNSIGNED")
        (neg-number "INTEGER")
        (float "REAL"))))

(defun as-value-comment (value-type)
  "If VALUE-TYPE is a list of more than one base type, return a string that serves
as an ECL comment describing those types."
  (when (and (consp value-type)
             (or (and (= (length value-type) 1)
                      (eql (car value-type) 'null-value))
                 (and (> (length value-type) 1)
                      (member (as-ecl-type value-type) '(*ecl-string-type* "STRING") :test #'string=))))
    (labels ((desc (v)
               (case v
                 (null-value "null")
                 (default-string "string")
                 (pos-number "unsigned integer")
                 (neg-number "signed integer")
                 (t (format nil "~(~A~)" v)))))
      (format nil "// ~{~A~^, ~}" (mapcar #'desc value-type)))))
;;;

(defun base-type (value)
  "Determine the basic internal data type of VALUE."
  (let ((value-str (format nil "~A" value))
        (found-type nil))
    (cond ((string= value-str "")
           (setf found-type 'default-string))
          ((member (string-downcase value-str) '("true" "false") :test #'string=)
           (setf found-type 'boolean))
          (t
           (loop named char-walker
                 for c across value-str
                 with pos = 0
                 with decimal-char-found-p = nil
                 do (progn
                      (cond ((and (eql c #\-) (zerop pos))
                             (setf found-type (common-type 'neg-number found-type)))
                            ((and (eql c #\+) (zerop pos))
                             (setf found-type (common-type 'pos-number found-type)))
                            ((digit-char-p c)
                             (setf found-type (common-type 'pos-number found-type)))
                            ((and (eql c #\.) (not decimal-char-found-p))
                             (setf decimal-char-found-p t
                                   found-type (common-type 'float found-type)))
                            (t
                             (setf found-type 'default-string)))
                      (incf pos)
                      (when (eql found-type 'default-string)
                        (return-from char-walker))))))
    found-type))

(defun common-type (new-type old-type)
  "Given two internal data types, return an internal type that can encompass both."
  (let ((args (list new-type old-type)))
    (cond ((not old-type)
           new-type)
          ((not new-type)
           old-type)
          ((eql new-type old-type)
           new-type)
          ((member 'default-string args)
           'default-string)
          ((member 'string args)
           'string)
          ((and (member 'neg-number args)
                (member 'pos-number args))
           'neg-number)
          ((and (intersection '(neg-number pos-number) args)
                (member 'float args))
           'float)
          (t
           'string))))

(defun reduce-base-type (types)
  (reduce #'common-type types))

;;;

(defgeneric as-ecl-field-def (value-obj name attributep)
  (:documentation "Create an ECL field definition from an object or array class."))

(defmethod as-ecl-field-def ((value-obj t) name attributep)
  (let* ((ecl-type (as-ecl-type value-obj))
         (xpath (as-ecl-xpath name attributep))
         (comment (as-value-comment value-obj))
         (field-def (with-output-to-string (s)
                      (format s "~4T~A ~A ~A;" ecl-type (as-ecl-field-name name) xpath)
                      (when comment
                        (format s " ~A" comment))
                      (format s "~%"))))
    field-def))

(defmethod as-ecl-field-def ((obj object-item) name attributep)
  (let* ((xpath (as-ecl-xpath name attributep))
         (field-def (with-output-to-string (s)
                      (format s "~4T~A ~A ~A" (as-dataset-type name) (as-ecl-field-name name) xpath)
                      (format s ";~%"))))
    field-def))

;;;

(defgeneric as-ecl-record-def (obj name)
  (:documentation "Create an ECL RECORD definition from an object or array class."))

(defmethod as-ecl-record-def ((obj t) name)
  (declare (ignore obj name))
  "")

(defmethod as-ecl-record-def ((obj object-item) name)
  (let* ((result-str "")
         (my-str (with-output-to-string (s)
                   (register-layout-subname name)
                   (format s "~A := RECORD" (as-layout-name name))
                   ; (format s " // ~A" (max-visit-count obj))
                   (format s "~%")
                   (loop for field-name being the hash-keys of (attrs obj)
                           using (hash-value field-value)
                         do (format s "~A" (as-ecl-field-def field-value field-name t)))
                   (loop for field-name being the hash-keys of (children obj)
                           using (hash-value field-value)
                         do (let ((child-recdef (as-ecl-record-def field-value field-name)))
                              (when (string/= child-recdef "")
                                (setf result-str (format nil "~A~A" result-str child-recdef)))
                              (format s "~A" (as-ecl-field-def field-value field-name nil))))
                   (format s "END;~%~%")
                   )))
    (format nil "~A~A" result-str my-str)))

;;;

(defun first-hash-table-key (hash-table)
  (loop for k being the hash-keys of hash-table
        do (return k)))

(defun first-hash-table-value (hash-table)
  (loop for v being the hash-values of hash-table
        do (return v)))

(defun first-hash-table-kv (hash-table)
  (loop for k being the hash-keys of hash-table
          using (hash-value v)
        do (return (values k v))))

(defun as-ecl-dataset-example (toplevel-obj toplevel-name toplevel-xpath)
  (let* ((child-obj (first-hash-table-value (children toplevel-obj)))
         (noroot-opt (if (and (eql (type-of child-obj) 'object-item)
                              (> (max-visit-count child-obj) 1)) ", NOROOT" ""))
         (result-str (with-output-to-string (s)
                       (format s "// ds := DATASET('~~data::~A', ~A, XML('~A'~A));~%~%"
                               (string-downcase toplevel-name)
                               (as-layout-name toplevel-name)
                               toplevel-xpath
                               noroot-opt))))
    result-str))

;;;

(defmacro reuse-object (place classname)
  "Return object found in PLACE if it is an instance of CLASSNAME, or create a
new instance of CLASSNAME in place and return that."
  `(progn
     (cond ((or (null ,place) (not ,place) (eql ,place 'null-value))
            (setf ,place (make-instance ,classname)))
           ((and (consp ,place) (eql (car ,place) 'null-value))
            (setf ,place (make-instance ,classname)))
           ((not (typep ,place ,classname))
            (error "xml2ecl: Mismatching object types; expected ~A but found ~A"
                   ,classname
                   (type-of ,place))))
     (incf (visit-count ,place))
     ,place))

(defmacro parse-simple (place value)
  "Pushes the base type of VALUE onto the sequence PLACE."
  `(unless (typep ,place 'object-item)
     (pushnew (base-type ,value) ,place)))

(defmacro parse-complex (place classname source)
  "Reuse object in PLACE if possible, or create a new instance of CLASSNAME,
then kick off a new depth of parsing with the result."
  `(progn
     (reuse-object ,place ,classname)
     (parse-attrs ,place ,source)
     (parse-obj ,place ,source)))

;;;

(defgeneric parse-attrs (obj source)
  (:documentation "Parses XML attributes and inserts base data types into OBJ."))

(defmethod parse-attrs ((obj object-item) source)
  (labels ((handle-attrs (ns local-name qualified-name value explicitp)
             (declare (ignore ns qualified-name))
             (when explicitp
               (parse-simple (gethash local-name (attrs obj)) value))))
    (fxml.klacks:map-attributes #'handle-attrs source)))

(defgeneric parse-obj (obj source)
  (:documentation "Parses XML tokens into an internal object representation."))

(defmethod parse-obj ((obj t) source)
  (loop named parse
        do (let ((event (fxml.klacks:peek source)))
             (cond ((null event)
                    (return-from parse))
                   ((eql event :end-document)
                    (return-from parse))
                   ((eql event :start-element)
                    (return-from parse (parse-obj (make-instance 'object-item) source)))
                   ((member event '(:start-document))
                    (fxml.klacks:consume source))
                   (t
                    (error "xml2ecl: Unknown event at toplevel: (~A)" event))))))

(defmethod parse-obj ((obj object-item) source)
  (loop named parse
        do (multiple-value-bind (event chars name) (fxml.klacks:consume source)
             (cond ((null event)
                    (return-from parse obj))
                   ((eql event :end-document)
                    (reset-children-visits obj)
                    (return-from parse obj))
                   ((eql event :start-element)
                    (parse-complex (gethash name (children obj)) 'object-item source))
                   ((eql event :end-element)
                    (reset-children-visits obj)
                    (return-from parse obj))
                   ((eql event :characters)
                    (let ((text (string-trim '(#\Space #\Tab #\Newline) (format nil "~A" chars))))
                      (unless (string= text "")
                        (parse-simple (gethash *inner-text-tag* (children obj)) text))))
                   ((member event '(:start-document))
                    ;; stuff to ignore
                    )
                   (t
                    (error "xml2ecl: Unknown event: (~A)" event))))))

;;;

(defmethod fixup ((obj t))
  obj)

(defmethod fixup ((obj object-item))
  (loop for name being the hash-keys of (children obj)
          using (hash-value child)
        do (if (single-inner-text-p child)
               (setf (gethash name (children obj)) '(default-string))
               (fixup child)))
  obj)

;;;

(defmacro with-wrapped-xml-stream ((s element-name wrapped-stream) &body body)
  "Wrap stream WRAPPED-STREAM, containing XML data, with empty tags named ELEMENT-NAME.
S should be the symbol of the stream that is created and will be referenced in the BODY."
  (let ((begin-tag-stream (gensym "begin_stream_"))
        (end-tag-stream (gensym "end_stream_"))
        (start-tag (gensym "start_tag_"))
        (end-tag (gensym "end_tag_")))
    `(let ((,start-tag (format nil "<~A>" ,element-name))
           (,end-tag (format nil "</~A>" ,element-name)))
       (flexi-streams:with-input-from-sequence (,begin-tag-stream ,start-tag :transformer #'char-code)
         (flexi-streams:with-input-from-sequence (,end-tag-stream ,end-tag :transformer #'char-code)
           (let ((,s (make-concatenated-stream ,begin-tag-stream ,wrapped-stream ,end-tag-stream)))
             ,@body))))))

(defun process-stream (input obj)
  (let ((wrapper-tag "wrapper"))
    (with-wrapped-xml-stream (input-stream wrapper-tag input)
      (fxml.klacks:with-open-source (source (fxml:make-source input-stream :buffering nil))
        (setf obj (parse-obj obj source)))))
  obj)

(defmethod process-file-or-stream (input obj)
  (with-open-file (file-stream (uiop:probe-file* input)
                               :direction :input
                               :element-type '(unsigned-byte 8))
    (process-stream file-stream obj)))

(defmethod process-file-or-stream ((input stream) obj)
  (process-stream input obj))

;;;

(defun unwrap-parsed-object (obj)
  (let ((top-obj (or (gethash "wrapper" (children obj)) obj))
        (top-xpath nil))
    (loop named unwrap
          do (multiple-value-bind (child-name child-obj) (first-hash-table-kv (children top-obj))
               (if (and (zerop (hash-table-count (attrs top-obj)))
                        (= (hash-table-count (children top-obj)) 1)
                        (eql (type-of child-obj) 'object-item))
                   (setf top-obj child-obj
                         top-xpath (with-output-to-string (s)
                                     (when top-xpath
                                       (format s "~A/" top-xpath))
                                     (format s "~A" child-name)))
                   (return-from unwrap))))
    (values top-obj top-xpath)))

