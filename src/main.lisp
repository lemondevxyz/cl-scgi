(defpackage :cl-scgi
  (:use :cl :cl-user))
(in-package :cl-scgi)

(ql:quickload '(:fast-io :flexi-streams :babel :str :alexandria) :silent t)

(defun list-of-p (τ thing)
  "list-of-p is a predicate that checks if a list is of that specific type."
  (and (listp thing) (every (lambda (x) (typep x τ)) thing)))
(deftype list-of (type)
  "list-of is a type that specifies a list of that exact *type*"
  (let ((predicate (gensym)))
    (setf (symbol-function predicate)
      #'(lambda (seq) (list-of-p type seq)))
    `(and list (satisfies ,predicate))))
(deftype positive-fixnum ()
  "positive-fixnum names all integers from 1 to *mostpositive-fixnum*"
  `(integer 1 ,most-positive-fixnum))

(defun list-to-vector (list)
  "list-to-vector is a function that converts a (unsigned-byte 8) list
 to a vector of that same type."
  (declare (type (list-of (unsigned-byte 8)) list))
  (let ((vec (make-array 0
                         :element-type '(unsigned-byte 8)
                         :initial-element 0
                         :fill-pointer 0)))
    (declare (type (vector (unsigned-byte 8) *) vec))
    (loop for x in list do
      (vector-push-extend x vec))
    vec))

(defun vector-to-list (vec)
  "vector-to-list is a function that converts a vector of type (unsigned-byte 8)
to a list of that same type"
  (let ((list nil))
    (loop for x from 0 below (length vec) do
      (setf list (append list (list (elt vec x)))))
    list))

;; (defun extract-header (str)
;;   "DEPRECATED extract-header is a function that extracts a header key or value string
;; and simultaneously checks if it's valid or not.
;;
;; Example: (extract-header \"\"CONTENT_LENGTH\"\") ; \"CONTENT_LENGTH\" "
;;   (unless (and (two-quotes-p str) (wrapped-in-quotes-p str))
;;     (error (format nil "header ~a must be wrapped in quotes" str)))
;;   (let ((ret (str:replace-all "\"" "" str)))
;;     (unless (> (length ret) 0)
;;       (error "header cannot be empty"))
;;     ret))
;; (export 'extract-header)

(defun parse-headers (str)
  "parse-headers is a function that parses all headers from a scgi request.
parse-headers returns a hash map because it is more performant than an associated list.

Example: \"\"CONTENT_LENGTH\" <00> \"27\" <00>
\"SCGI\" <00> \"1\" <00>\"

Note: <00> is the NUL character"
  (declare (string str))
  (let ((split (str:split (format nil "~a" #\Nul) str)))
    (unless (or (zerop (length split)) (oddp (length split)))
      (error "bad formatting; string must have an extra NUL character"))
    (let ((hash (make-hash-table :test #'equal) ))
      (loop for index from 0 below (length split) by 2 do
        (when (< (1+ index) (length split))
          (setf (gethash (elt split index) hash)
                (elt split (1+ index)))))
      hash)))
(export 'parse-headers)

(defun parse-header (str)
  "DEPRECATED parse-header parses a scgi header. scgi headers are marked by their
NUL character after both the key and the value.

Example: \"\\\"CONTENT_LENGTH\\\"<00>\\\"27\\\"<00>\"

Note: <00> is the NUL character

parse-header will throw an error if the key or value weren't wrapped in
quotes."
  (let* ((ret (parse-headers str))
         (key
           (ignore-errors (elt (alexandria:hash-table-keys ret) 0))))
    (if key
        (values key (gethash key ret))
        nil)))
(export 'parse-header)

(defun list-to-header (list)
  "list-to-header creates client-like(web-server) headers through a list.

the list must be a list of strings and of even length"
  (declare (type (list-of string) list))
  (unless (evenp (length list)) (error "list must be even"))
  (let ((str ""))
    (loop for x from 0 below (length list) by 2
          do
             (setf str (format nil "~a~a~a~a~a" str (elt list x) #\Nul (elt list (1+ x)) #\Nul)))
    str))

(defun parse-binary-header (binary)
  "parse-binary-header is a function that parses headers represented in
vector of octets"
  (declare (type (vector (unsigned-byte 8) *) binary))
  (parse-headers (babel:octets-to-string binary)))
(export 'parse-binary-header)

(defun parse-request-from-stream (stream)
  "parse-request-from-stream is a function that parses a normal SCGI request
from a stream.

SCGI requests are composed of 4 parts:
1. the header length + seperator(:)
2. the header content
3. the body seperator (,)
4. the body content

parse-request-from-stream is quite strict in its design; several checks are made
and aren't left for the developer. For example, the header \"CONTENT_LENGTH\"
*must* match the size of the body. Otherwise, extra content ends being
unused but no error is thrown.

Furthermore, parse-request doesn't make any effort to parse the headers,
instead it restricts itself to only seperating the body and header from
the rest of the request, giving the developer complete freedom over how
to deal with the request.

parse-request throws an error in one of these cases:
1. comma isn't present in the packets..
2. the header length is bigger than the request itself
3. the request is malformed in the case of a comma not being present
   after the last header
4. the given header length is bigger than the actual header length"
  (declare (stream stream))
  (let* ((header-len-str (make-array 0 :fill-pointer 0 :adjustable t
                                       :element-type 'character))
         (header-len 0)
         (content-len-str (make-array 0 :fill-pointer 0 :adjustable t
                                        :element-type 'character))
         (content-len 0)
         (header-buf (fast-io:make-octet-vector 0)))
    (declare (number header-len content-len))
    (declare (type (vector character *) header-len-str content-len-str))
    (fast-io:with-fast-input (buffer nil stream)
      (loop for byte = (fast-io:fast-read-byte buffer)
            until (eq byte #x3a) do
              (vector-push-extend (code-char byte) header-len-str))
      (setf header-len (parse-integer header-len-str))
      (setf header-buf (fast-io:make-octet-vector header-len))
      (fast-io:fast-read-sequence header-buf buffer 0 header-len)
      (unless (equal (fast-io:fast-read-byte buffer) #x2c)
        (error "malformed request; comma must be after headers"))
      (loop for x from 15 below (length header-buf)
            until (= content-len -1) do
              (let ((byte (elt header-buf x)))
                (if (= byte 0)
                    (setf content-len -1)
                    (vector-push-extend (code-char byte) content-len-str))))
      (setf content-len (or (ignore-errors (parse-integer content-len-str)) 0)))
    (values header-buf content-len)))
(export 'parse-request-from-stream)

(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))
(defun read-until-content-length (content-len stream)
  "read-until-content-length reads from the stream until it reaches content-len"
  (declare (positive-fixnum content-len))
  (declare (stream stream))
  (let ((vec (make-array 1 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (declare (type (vector (unsigned-byte 8) *) vec))
    (loop for x from 0 below content-len do
      (vector-push-extend (read-byte stream) vec))
    vec))
(export 'read-until-content-length)

(defun parse-request (request)
  "DEPRECATED parse-request is a function that wraps around parse-request-from-stream
to make it easier to test out parsing requests and also to satsify old users
of the library.

It takes in a vector of unsigned bytes of size 8 (octet) and returns two
vectors of octets in multi-value fashion those values are the header
and the body. Do note that these aren't parsed and can be parsed with helper
functions like `(parse-header (babel:octets-to-string headers))' and
`(babel:octets-to-string body)'"
  (declare (type (vector (unsigned-byte 8) *) request))
  (let ((stream (flexi-streams:make-in-memory-input-stream request)))
    (multiple-value-bind (headers content-len)
        (parse-request-from-stream stream)
      (values headers (read-until-content-length content-len stream)))))
(export 'parse-request)

(defun parse-request-with-headers (request)
  "DEPRECATED parse-request-with-headers is a wrapper around parse-headers that parses
the headers and returns them in a hash table

Note: this function, by itself, could throw an error if a utf-8 character
is invalid. To further debug this issue, see babel:octets-to-string"
  (declare (type (vector (unsigned-byte 8) *) request))
  (multiple-value-bind (headers body)
      (parse-request request)
    (values (parse-headers (babel:octets-to-string headers)) body)))
(export 'parse-request-with-headers)

(defun parse-request-as-string (request)
  "DEPRECATED parse-request-as-string is a wrapper around parse-request-with-headers
with one key difference: the body is returned as a string.

Note: this function, by itself, could throw an error if a utf-8 character
is invalid. To further debug this issue, see babel:octets-to-string"
  (declare (type (vector (unsigned-byte 8) *) request))
  (multiple-value-bind (headers body)
      (parse-request-with-headers request)
    (values headers (babel:octets-to-string body))))
(export 'parse-request-as-string)
