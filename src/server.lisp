(in-package :cl-scgi)

(ql:quickload '(:alexandria) :silent t)

;; `server.lisp' is basically a file that's shared between
;; `unix.lisp' and `tcp.lisp'.

(deftype request-callback-type ()
  `(function ((vector (unsigned-byte 8))
              integer
              stream)))

(defun write-bytes (seq stream)
  "write-bytes is a function that writes sequences to a stream that only
supports write-byte."
  (declare (type (vector (unsigned-byte 8)) seq))
  (declare (type stream stream))
  (loop for x from 0 below (length seq) do
    (write-byte (elt seq x) stream)))
(export 'write-bytes)

(declaim (type (or request-callback-type null) *continue-callback*))
(defvar *continue-callback* nil
  "*continue-callback* is a custom function that can be set by outside packages.
*continue-callback* is executed whenever a request gets derailed and the user chooses
CONTINUE-AND-WRITE.")

(defun prompt-new-value (prompt)
  "prompt-new-value is a function that prompts Common Lisp's console for
a value to be inputted by the user."
  (format *query-io* prompt)
  (force-output *query-io*)
  (eval (read *query-io*)))

(defun read-until-eof (stream)
  "DEPRECATED read-until-eof reads a stream until it reaches EOF and then returns
a vector of all bytes it read."
  (declare (stream stream))
  (let ((read-input (make-array 0 :element-type '(unsigned-byte 8)
                                  :initial-element 0
                                  :fill-pointer 0))
        (byte nil))
    (declare (type (vector (unsigned-byte 8) *) read-input))
    (declare (type (or integer null (eql :eof)) byte))
    (loop while (not (equal byte :eof)) do
      (setf byte (read-byte stream nil :eof))
      (format t " read ~a " byte)
      (unless (equal byte :eof)
        (vector-push-extend byte read-input)))
    read-input))
(export 'read-until-eof)

(defvar *continue* nil
  "*continue* is an usused global variable. it is mainly defined to allow
`parse-request-or-restart' to stop unix-server or tcp-server from receiving
requests.")

(defun parse-request-or-restart (stream fn)
  "parse-request-or-restart is a function that wraps around parse-request.
basically, it provides four useful restart functions.

1. STOP: stops the server from listening anymore and closes the client's connection
2. CONTINUE-AND-CLOSE: continues listening but closes the client's connection
3. CONTINUE-AND-EXEC-CALLBACK: continues listening and evaluates
   *continue-callback* for the client's connection
4. CONTINUE-AND-EVAL: continues listening and evaluates a custom response
   for the client's request. Do note that this callback will only take
   in a writable stream without a body or header"
  (declare (stream stream))
  (declare (request-callback-type fn))
  ;; because, for some reason (close stream)
  ;; doesn't update right away, so you have to
  ;; use another variable to close the stream
  (let ((closed nil))
    (unwind-protect
         (restart-case (multiple-value-bind (headers content-length)
                           (parse-request-from-stream stream)
                         (funcall fn headers content-length stream))
           (stop () :report "Stop accepting connections"
             (setf *continue* nil)
             (when (open-stream-p stream)
               (close stream)
               (setf closed t)))
           (continue-and-close () :report "Continue and close this request"
             (setf *continue* t)
             (when (open-stream-p stream)
               (close stream)
               (setf closed t)))
           (continue-and-exec-callback ()
             :report "Continue and execute *continue-callback*"
             :test (lambda () (not (null *continue-callback*)))
             (funcall *continue-callback* stream))
           (continue-and-eval ()
             :report "Continue and eval an expression"
             :interactive
             (lambda ()
               (prompt-new-value (format nil "Enter an S-Expression~%")))))
      (when (and (open-stream-p stream) (null closed))
        (finish-output stream)
        (close stream)))))

(defun format-headers (headers)
  "format-headers is a function that turns a hash-table into a SCGI-compatible
header response."
  (declare (hash-table headers))
  (let ((keys (alexandria:hash-table-keys headers))
        (headers-str "")
        (bytes (make-array 1 :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (loop for key in keys do
      (setf headers-str
            (format nil "~a~a: ~a~a~a" headers-str key (gethash key headers) #\return #\newline)))
    (when (> (length headers-str) 0)
      (let ((octets (babel:string-to-octets headers-str)))
        (loop for x from 0 below (length octets) do
          (vector-push-extend (elt octets x) bytes))))
    (vector-push-extend (char-code #\Return) bytes)
    (vector-push-extend (char-code #\Newline) bytes)
    bytes))
(export 'format-headers)

(defun response-string (headers body stream)
  "response-string is a function that sends a string body with a hash-table
of headers."
  (declare (stream stream))
  (declare (hash-table headers))
  (declare (string body))
  (write-bytes (format-headers headers) stream)
  (write-bytes (babel:string-to-octets body) stream))
(export 'response-string)
