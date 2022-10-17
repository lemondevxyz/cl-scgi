(ql:quickload '(:bordeaux-threads :unix-sockets :trivial-gray-streams) :silent t)

(in-package :cl-scgi)
;; requires server.lisp to work

(defun unix-server (path fn &key ((:multi-threaded mt) t))
  "unix-server is a function that creates a unix socket, accepts connections,
parses the request(error-prone) and finally calls fn to handle the request."
  (declare (string path))
  (declare (request-callback-type fn))
  (declare (type (or t nil) mt))
  (when (probe-file path) (delete-file path))
  (unix-sockets:with-unix-socket
      (sock (unix-sockets:make-unix-socket path))
    (unwind-protect
         (let* ((*continue* t))
           (loop while *continue* do
             (let* ((conn (unix-sockets:accept-unix-socket sock))
                    (stream (unix-sockets:unix-socket-stream conn)))
               (if mt
                   (bt:make-thread
                    (lambda ()
                      (parse-request-or-restart (read-until-eof stream) stream fn)))
                   (parse-request-or-restart (read-until-eof stream) stream fn)))))
      (when (probe-file path) (delete-file path)))))

(defvar *msg* (babel:string-to-octets "hello world!"))
(defun generic-callback (headers body stream)
  (declare (ignorable headers))
  (declare (ignorable body))
  (declare (flexi-streams:flexi-io-stream stream))
  (declare (type (vector (unsigned-byte 8)) headers body))
  (write-bytes body stream)
  (force-output stream))

(export 'unix-server)
