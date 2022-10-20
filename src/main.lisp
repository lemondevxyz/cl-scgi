(defpackage :cl-scgi
  (:use :cl :cl-user))
(in-package :cl-scgi)

(ql:quickload '(:flexi-streams :babel :str :alexandria) :silent t)

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
(deftype ascii-number ()
  "DEPRECATED ascii-number is a number from 30 to 39"
  `(integer #x30 #x39))

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

(defun number-to-ascii-bytes (num)
  "DEPRECATED number-to-ascii-bytes converts a number to ascii bytes that are used
for net code.

Example: (number-to-ascii-bytes 123456789) ;; => (31 32 33 34 35 36 37 38 39)"
  (declare (positive-fixnum num))
  (let ((arr (list-to-vector '()))
        (strnum (format nil "~a" num)))
    (declare (type (vector (unsigned-byte 8) *) arr))
    (declare (string strnum))
    (loop for index from 0 below (length strnum) do
      (let* ((digit (elt strnum index))
             (ascii (+ (digit-char-p digit) #x30)))
        (declare (character digit))
        (declare (ascii-number ascii))
        (vector-push-extend ascii arr)))
    arr))
(defun ascii-bytes-to-number (bytes)
  "DEPRECATED ascii-bytes-to-number converts an array of bytes to a number that's used
for net code.

Example: (ascii-bytes-to-number '(31 32 33 34 35 36 37 38 39)) ;; => 123456789"
  (declare (type (vector (unsigned-byte 8) *) bytes))
  (let* ((len (length bytes))
         (num 0))
    (loop for index from 0 below len do
      (let ((byte (elt bytes index)))
        (unless (and (>= byte #x30) (<= byte #x39))
          (error "byte must be between 30 and 39"))
        (let* ((int (- byte #x30))
               (place (1- (- len index))))
          (setf num (+ (* (expt 10 place) int) num)))))
    num))

(export 'ascii-bytes-to-number)
(export 'number-to-ascii-bytes)

(defun != (a b)
  "DEPRECATED != is the inverse of ="
  (not (= a b)))

(defun wrapped-in-char-p (str char)
  "DEPRECATED wrapped-in-char-p is a predicate that returns T if a string has char
in the start and end"
  (declare (string str))
  (declare (character char))
  (and
   (equal (elt str 0) char)
   (equal (elt str (1- (length str))) char)))

(defun wrapped-in-quotes-p (str)
  "DEPRECATED wrapped-in-quotes is a predicate that returns T if a string has quotes
at the start and end."
  (declare (string str))
  (wrapped-in-char-p str #\"))

(defun two-quotes-p (str)
  "two-quotes-p is a predicate that makes sure a string
*only* has two quotes."
  (declare (string str))
  (= (str:count-substring "\"" str) 2))

(defun extract-header (str)
  "DEPRECATED extract-header is a function that extracts a header key or value string
and simultaneously checks if it's valid or not.

Example: (extract-header \"\"CONTENT_LENGTH\"\") ; \"CONTENT_LENGTH\" "
  (unless (and (two-quotes-p str) (wrapped-in-quotes-p str))
    (error (format nil "header ~a must be wrapped in quotes" str)))
  (let ((ret (str:replace-all "\"" "" str)))
    (unless (> (length ret) 0)
      (error "header cannot be empty"))
    ret))
(export 'extract-header)

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

(defun parse-request-from-stream (stream)
  "parse-request-from-stream is a function that parses a normal SCGI request
from a stream.

SCGI requests are composed of 4 parts:
1. the header length + seperator(:)
2. the header content
3. the body seperator
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
  (let* ((header-len-str "")
         (header-len 0)
         (headers-bytes (make-array 1 :element-type '(unsigned-byte 8) :fill-pointer 0))
         (content-len-str "")
         (content-len 0))
    (loop for byte = (read-byte stream)
          until (eq byte #x3a) do
            (setf header-len-str (format nil "~a~a" header-len-str (code-char byte))))
    (setf header-len (parse-integer header-len-str))
    (loop for x from 0 below header-len do
      (vector-push-extend (read-byte stream) headers-bytes))
    (unless (equal (read-byte stream) #x2c)
      (error "malformed request; comma must be after headers"))
    (loop for x from 15 below (length headers-bytes)
          until (= content-len -1) do
            (let ((byte (elt headers-bytes x)))
              (if (= byte 0)
                  (setf content-len -1)
                  (setf content-len-str (format nil "~a~a" content-len-str (code-char byte))))))
    (setf content-len (parse-integer content-len-str))
    (values headers-bytes content-len)))
(export 'parse-request-from-stream)

(deftype positive-fixnum ()
  `(integer 0 ,most-positive-fixnum))
(defun read-until-content-length (content-len stream)
  "read-until-content-length reads from the stream until it reaches content-len"
  (declare (positive-fixnum content-len))
  (declare (stream stream))
  (let ((vec (make-array 1 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (print content-len)
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

;; (defun parse-request (request)
;;   "DEPRECATED parse-request is a function that parses a normal SCGI request.
;;
;; SCGI requests are composed of 4 parts:
;; 1. the header length + seperator(:)
;; 2. the header content
;; 3. the body seperator
;; 4. the body content
;;
;; parse-request is lenient in its design; several checks are not made
;; and are left for the developer. For example, the header \"CONTENT_LENGTH\"
;; doesn't *have to* match the size of the body.
;;
;; Furthermore, parse-request doesn't make any effort to parse the headers,
;; instead it restricts itself to only seperating the body and header from
;; the rest of the request, giving the developer complete freedom over how
;; to deal with the request.
;;
;; parse-request throws an error in one of these cases:
;; 1. comma isn't present in the packets..
;; 2. the header length is bigger than the request itself
;; 3. the request is malformed in the case of a comma not being present
;;   after the last header
;; 4. the given header length is bigger than the actual header length"
;;   (declare (type (vector (unsigned-byte 8) *) request))
;;   (let* ((found nil)
;;         (len-bytes (list-to-vector '()))
;;         (len-header 0)
;;         (header-start -1)
;;         (header-end -1)
;;         (headers ""))
;;     (declare (type (or t nil) found))
;;     (declare (type (vector (unsigned-byte 8) *) len-bytes))
;;     (let ((index 0))
;;       (loop while (not found) do
;;         (when (> index (length request))
;;           (error "index too big"))
;;         (when (= (elt request index) #x3a) (setf found t))
;;         (unless found
;;           (vector-push-extend (elt request index) len-bytes)
;;           (setf index (1+ index))))
;;       (setf len-header (ascii-bytes-to-number len-bytes))
;;       (setf header-start (+ index 1))
;;       (setf header-end (+ header-start len-header)))
;;     (when (> header-end (length request))
;;       (error "malformed request; header length is bigger than request itself"))
;;     (setf headers (subseq request header-start header-end))
;;     (unless (= (elt request header-end) #x2c)
;;       (error "malformed request; bytes after headers must be #x2c: ~a" (elt request header-end)))
;;     (let ((end (+ header-end 1)))
;;       (when (>= end (length request))
;;         (setf end (1- (length request))))
;;       (values headers (subseq request end (length request))))))

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
