(ql:quickload '(:bordeaux-threads :unix-sockets) :silent t)

(in-package :cl-scgi)
;; requires server.lisp to work

(defun unix-server (path fn &key ((:multi-threaded mt) t)
                              ((:backlog bl) 128))
  "unix-server is a function that creates a unix socket, accepts connections,
parses the request(error-prone) and finally calls fn to handle the request."
  (declare (string path))
  (declare (request-callback-type fn))
  (declare (type (or t nil) mt))
  (when (probe-file path) (delete-file path))
  (unix-sockets:with-unix-socket
      (sock (unix-sockets:make-unix-socket path :backlog bl))
    (unwind-protect
         (let* ((*continue* t))
           (loop while *continue* do
             (let* ((conn (unix-sockets:accept-unix-socket sock))
                    (stream (unix-sockets:unix-socket-stream conn)))
               (if mt
                   (bt:make-thread
                    (lambda ()
                      (parse-request-or-restart stream fn)
                      (unix-sockets:close-unix-socket sock)))
                   (progn
                     (parse-request-or-restart stream fn))))))
      (when (probe-file path) (delete-file path))))
  (when (probe-file path) (delete-file path)))
(export 'unix-server)
