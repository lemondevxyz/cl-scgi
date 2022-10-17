(in-package :cl-scgi)

(ql:quickload :flexi-streams :silent t)

(deftype request-callback-type ()
  `(function ((vector (unsigned-byte 8))
              (vector (unsigned-byte 8))
              flexi-streams:flexi-io-stream)))

(defun write-bytes (seq stream)
  "write-bytes is a function that writes sequences to a stream that only
supports write-byte."
  (declare (type (vector (unsigned-byte 8)) seq))
  (declare (trivial-gray-streams:fundamental-binary-output-stream stream))
  (loop for x from 0 below (length seq) do
    (trivial-gray-streams:stream-write-byte stream (elt seq x))))
(export 'write-bytes)

(declaim (type (or request-callback-type null) *continue-callback*))
(defvar *continue-callback* nil
  "*continue-callback* is a custom function that can be set by outside packages.
*continue-callback* is executed whenever a request gets derailed and the user chooses
CONTINUE-AND-WRITE.")

(defun prompt-new-value (prompt)
  (format *query-io* prompt)
  (force-output *query-io*)
  (eval (read *query-io*)))

(defun read-until-eof (stream)
  (declare (trivial-gray-streams:fundamental-binary-input-stream stream))
  (let ((read-input (make-array 0 :element-type '(unsigned-byte 8)
                                  :initial-element 0
                                  :fill-pointer 0))
        (byte nil))
    (declare (type (vector (unsigned-byte 8) *) read-input))
    (declare (type (or integer null (eql eof)) byte))
    (loop while (not (equal byte 'eof)) do
      (setf byte (read-byte stream nil 'eof))
      (unless (equal byte 'eof)
        (vector-push-extend byte read-input)))
    read-input))

(defun handle-request-or-restart (stream fn)
  (let ((req (read-until-eof stream)))
    (multiple-value-bind (headers body)
        (parse-request req)
      (funcall fn headers body stream))))

(defvar *continue* nil)

(defun parse-request-or-restart (seq stream fn)
  "parse-request-or-restart is a function that wraps around parse-request.
basically, it provides four useful restart functions.

1. STOP: stops the server from listening anymore and closes the client's connection
2. CONTINUE-AND-CLOSE: continues listening but closes the client's connection
3. CONTINUE-AND-EXEC-CALLBACK: continues listening and evaluates
   *continue-callback* for the client's connection
4. CONTINUE-AND-EVAL: continues listening and evaluates a custom response
   for the client's request. Do note that this callback will only take
   in a writable stream without a body or header"
  (declare (type (vector (unsigned-byte 8)) seq))
  (declare (trivial-gray-streams:fundamental-binary-output-stream stream))
  (declare (request-callback-type fn))
  ;; because, for some reason (close stream)
  ;; doesn't update right away, so you have to
  ;; use another variable to close the stream
  (let ((closed nil))
    (unwind-protect
         (restart-case (multiple-value-bind (headers body)
                           (parse-request seq)
                         (apply fn (list headers body stream)))
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
      (when (and (open-stream-p stream) (null closed)) (close stream)))))
