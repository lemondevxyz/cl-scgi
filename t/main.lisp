(in-package :fiveam)

(defun list-to-header-string (lst)
  (declare (list lst))
  (let ((str ""))
    (declare (string str))
    (loop for x from 0 below (length lst) by 2 do
      (setf str
            (format nil "~a\"~a\"~a\"~a\"~a"
                    str
                    (elt lst x) #\Nul
                    (elt lst (1+ x)) #\Nul)))
    str))

(def-suite* :cl-scgi)
(test extract-header
  ;; extract proper header
  (is (equal "CONTENT_LENGTH" (cl-scgi:extract-header "\"CONTENT_LENGTH\"")))
  ;; signal error for invalid header formattings
  (loop for x in '("CONTENT_LENGTH" "\"CONTENT_LENGTH" "\"CONTENT\"_LENGTH\"" "\"\"" )
        do
           (signals simple-error (cl-scgi:extract-header x))))

(test parse-header
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
  (let ((vec (make-array 0 :fill-pointer 0 :element-type '(unsigned-byte 8)))
        (header-len (cl-scgi:number-to-ascii-bytes
                     (length
                      (babel:string-to-octets (list-to-header-string headers)))))
        (header-bytes (babel:string-to-octets (list-to-header-string headers)))
        (body-bytes (babel:string-to-octets body)))
    (loop for x from 0 below (length header-len) do
      (vector-push-extend (elt header-len x) vec))
    (vector-push-extend #x3a vec)
    (loop for x from 0 below (length header-bytes) do
      (vector-push-extend (elt header-bytes x) vec))
    (vector-push-extend #x2c vec)
    (loop for x from 0 below (length body-bytes) do
          (vector-push-extend (elt body-bytes x) vec))
    vec))

(test parse-request
  (let* ((headers '("CONTENT_TYPE" "text/html"))
         (body "HELLO WORLDIE")
         (headers-bytes (babel:string-to-octets (list-to-header-string headers)))
         (body-bytes (babel:string-to-octets body)))
    (multiple-value-bind (new-headers new-body)
        (cl-scgi:parse-request (request-format headers body))
      (is (equal (babel:octets-to-string body-bytes) (babel:octets-to-string new-body)))
      (is (equal (babel:octets-to-string headers-bytes) (babel:octets-to-string new-headers))))))