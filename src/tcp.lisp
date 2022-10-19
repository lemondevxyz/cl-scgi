(in-package :cl-scgi)

(ql:quickload '(:usocket :bordeaux-threads) :silent t)
;; requires server.lisp to work

(defun tcp-server (host port fn &key
                                  ((:reuse-address ra) t)
                                  ((:multi-threaded mt) t))
  "tcp-server is a function that listens on a tcp socket, accepts connections,
parses the request(error-prone) and finally calls fn to handle the request."
  (declare (string host))
  (declare (type (integer 1 65535) port))
  (declare (request-callback-type fn))
  (declare (type (or t null) ra mt))
  (let* ((sock nil))
    (declare (type (or null usocket:stream-server-usocket) sock))
    (unwind-protect
         (let ((*continue* t)
               (conn nil)
               (stream nil))
           (declare (type (or null usocket:stream-usocket) conn))
           (declare (type (or null stream) stream))
           (setf sock
                 (usocket:socket-listen host port :reuse-address ra :element-type '(unsigned-byte 8)))
           (loop while *continue* do
             (setf conn (usocket:socket-accept sock :element-type '(unsigned-byte 8)))
             (setf stream (usocket:socket-stream conn))
             (if mt
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (parse-request-or-restart stream fn)))
                 (parse-request-or-restart stream fn))))
      (usocket:socket-close sock))))
