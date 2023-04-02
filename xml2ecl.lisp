;;;; xml2ecl.lisp

(in-package #:xml2ecl)

(declaim (optimize (debug 3)))

;;;

(defvar *layout-names* nil
  "Used while ECL record definitions are being emitted.  Tracks the names
of the record definitions created, so that subsequent creations don't reuse
previously-defined names.")

(defparameter *ecl-string-type* "UTF8"
  "The ECL data type to be used for XML string types.  Can be overridden
with an option.")

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

(defun type-of-value (value)
  "Determine the basic internal data type of VALUE."
  (let ((value-str (format nil "~A" value))
        (neg-char-found-p nil)
        (decimal-char-found-p nil)
        (found-type nil))
    (cond ((string= value-str "")
           (setf found-type 'default-string))
          ((member (string-downcase value-str) '("true" "false" "1" "0") :test #'string=)
           (setf found-type 'boolean))
          (t
           (loop named char-walker
                 for c across value-str
                 do (progn
                      (cond ((and (eql c #\-) (not neg-char-found-p))
                             (setf neg-char-found-p t
                                   found-type (common-type 'neg-number found-type)))
                            ((digit-char-p c)
                             (setf found-type (common-type 'pos-number found-type)))
                            ((and (eql c #\.) (not decimal-char-found-p))
                             (setf decimal-char-found-p t
                                   found-type (common-type 'float found-type)))
                            (t
                             (setf found-type 'default-string)))
                      (when (eql found-type 'default-string)
                        (return-from char-walker))))))
    found-type))

(defun output-attrs (ns local-name qualified-name value explicitp)
  (declare (ignore ns qualified-name))
  (when explicitp
    (format t "~4T~A = ~A (~A)~%" local-name value (type-of-value value))))

(defun process-file-or-stream (input)
  (with-open-file (file-stream (uiop:probe-file* input)
                               :direction :input
                               :element-type '(unsigned-byte 8))
    (with-wrapped-xml-stream (input-stream "bogus2" file-stream)
        (fxml.klacks:with-open-source (x (fxml:make-source input-stream :buffering nil))
          (loop named parse
                do (multiple-value-bind (event arg1 arg2 arg3) (fxml.klacks:consume x)
                     (when (null event)
                       (return-from parse))
                     (format t "~A: ~A,~A,~A~%" event arg1 arg2 arg3)
                     (when (eql event :start-element)
                       (fxml.klacks:map-attributes #'output-attrs x)))))))
  t)

