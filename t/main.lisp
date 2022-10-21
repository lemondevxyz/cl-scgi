(ql:quickload '(:flexi-streams :fast-io) :silent t)
(in-package :fiveam)

(defun list-to-header-string (lst)
  (declare (list lst))
  (let ((str ""))
    (declare (string str))
    (loop for x from 0 below (length lst) by 2 do
      (setf str
            (format nil "~a~a~a~a~a"
                    str
                    (elt lst x) #\Nul
                    (elt lst (1+ x)) #\Nul)))
    str))

(def-suite* header-suite)
(in-suite header-suite)
(test extract-header
  ;; DEPRECATED
  ;; extract proper header
  (is (equal "CONTENT_LENGTH" (cl-scgi:extract-header "\"CONTENT_LENGTH\"")))
  ;; signal error for invalid header formattings
  (loop for x in '("CONTENT_LENGTH" "\"CONTENT_LENGTH" "\"CONTENT\"_LENGTH\"" "\"\"" )
        do
           (signals simple-error (cl-scgi:extract-header x))))

(test parse-header
  ;; DEPRECATED
  ;; parse proper header
  (is (equal
       (multiple-value-list (cl-scgi:parse-header
                             (list-to-header-string '("CONTENT_LENGTH" "40"))))
       '("CONTENT_LENGTH" "40")))
  ;; empty header is still valid
  (finishes (cl-scgi:parse-header ""))
  (signals simple-error
    (cl-scgi:parse-header (format nil "\"~a\"~a\"~a\"" "CONTENT_LENGTH" #\Nul "40"))))

(test parse-headers
  (is (equal
       (alexandria:hash-table-plist (cl-scgi:parse-headers
                                     (list-to-header-string '("CONTENT_LENGTH" "40"))))
       '("CONTENT_LENGTH" "40")))
  (finishes (cl-scgi:parse-headers "")))

(defun request-format (headers body)
  (declare (list headers))
  (declare (string body))
  (let ((header-len (cl-scgi:number-to-ascii-bytes
                     (length
                      (babel:string-to-octets (list-to-header-string headers)))))
        (header-bytes (babel:string-to-octets (list-to-header-string headers)))
        (body-bytes (babel:string-to-octets body)))
    (concatenate '(vector (unsigned-byte 8)) header-len #(#x3a) header-bytes #(#x2c) body-bytes)))

(defmacro multi-bind1 (&body body)
  `(let* ((headers '("CONTENT_LENGTH" "13"))
          (body "HELLO WORLDIE")
          (headers-bytes (babel:string-to-octets (list-to-header-string headers)))
          (body-bytes (babel:string-to-octets body))
          (stream (flexi-streams:make-in-memory-input-stream (request-format headers body))))
     (multiple-value-bind (v1 v2)
         ,@body)))

(def-suite* parse-request)
(in-suite parse-request)
(test parse-request-from-stream
  (multi-bind1
    (cl-scgi:parse-request-from-stream stream)
    (is (equal (babel:octets-to-string headers-bytes) (babel:octets-to-string v1)))
    (is (equal
         (babel:octets-to-string
          (cl-scgi:read-until-content-length v2 stream))
         (babel:octets-to-string body-bytes)))))

(defun use (&rest args)
  (null args)
  nil)

(test parse-request
  ;; DEPRECATED
  (multi-bind1 (cl-scgi:parse-request (request-format headers body))
    (null stream)
    (is (equal (babel:octets-to-string body-bytes) (babel:octets-to-string v2)))
    (is (equal (babel:octets-to-string headers-bytes) (babel:octets-to-string v1)))))

(test parse-request-with-headers
  ;; DEPRECATED
  (multi-bind1 (cl-scgi:parse-request-with-headers (request-format headers body))
    (use stream body-bytes headers-bytes v2)
    (is-false (null (gethash "CONTENT_LENGTH" v1)))))

(test parse-request-as-string
  ;; DEPRECATED
  (multi-bind1 (cl-scgi:parse-request-as-string (request-format headers body))
    (use stream body-bytes headers-bytes)
    (is (equal v2 "HELLO WORLDIE"))
    (is-false (null (gethash "CONTENT_LENGTH" v1)))))

(def-suite* read-fn)
(in-suite read-fn)

(test read-until-eof
  (let* ((list (babel:string-to-octets "PUT THESE IN BYTES"))
         (vec (concatenate '(vector (unsigned-byte 8)) list))
         (stream (flexi-streams:make-in-memory-input-stream vec)))
    (is (babel:octets-to-string (cl-scgi:read-until-eof stream))
        (babel:octets-to-string vec))))
(test read-until-content-length
  (let* ((list (babel:string-to-octets "PUT THESE IN BYTES"))
         (vec (concatenate '(vector (unsigned-byte 8)) list))
         (stream (flexi-streams:make-in-memory-input-stream vec)))
    (is
     (babel:octets-to-string
      (cl-scgi:read-until-content-length (length list) stream))
     (babel:octets-to-string vec))))

(def-suite* response-writing)
(in-suite response-writing)

(test write-bytes
  (let* ((list (babel:string-to-octets "PUT THESE IN BYTES"))
         (vec (concatenate '(vector (unsigned-byte 8)) list))
         (stream (flexi-streams:make-in-memory-output-stream)))
    (cl-scgi:write-bytes vec stream)
    (is (equal (babel:octets-to-string (flexi-streams:get-output-stream-sequence stream)) (babel:octets-to-string vec)))))
(test format-headers
  (let* ((hash (make-hash-table :test 'equal)))
    (setf (gethash "cab" hash) "321")
    (setf (gethash "bac" hash) "123")
    (is (equal
         (babel:octets-to-string (cl-scgi:format-headers hash))
         (format nil "bac: 123~a
cab: 321~a
~a
" #\Return #\Return #\Return)))))
(test response-string
  (let* ((hash (make-hash-table :test 'equal))
         (body "hello world")
         (stream (flexi-streams:make-in-memory-output-stream)))
    (setf (gethash "cab" hash) "321")
    (setf (gethash "bac" hash) "123")
    (cl-scgi:response-string hash body stream)
    (is (equal
         (babel:octets-to-string (flexi-streams:get-output-stream-sequence stream))
         (format nil "bac: 123~a
cab: 321~a
~a
hello world" #\Return #\Return #\Return)))))
