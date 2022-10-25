## cl-scgi
cl-scgi is a library server implementation of the simple common gateway interface or SCGI. SCGI, as you could tell by the name, is extremely simple and easy to implement in most languages.

First, to get you up to speed with which web-servers support SCGI. Currently, there is:
- nginx
- lighttpd
- Caddy (through outside plugins)
- ashd

In-order to use SCGI, you need a web-server that supports SCGI. Afterwards, load the library and start using it.

## how-to: unix-server and tcp-server
The library works by using one of two functions, you could either use `tcp-server` or `unix-server`. Each has its own unique parameters, a path for the unix server or a port and ip for the tcp server.

However, both functions have "shared" parameters in the shape of keys, notably:
1. `multi-threaded`: whether or not each request spawns a new thread
2. `backlog`: how many connections are handled simultaneously

## example
Here is an app that prints to console the headers and the body string and writes "asd" to the HTTP client.
```common-lisp
(defun callback ()
     (declare (type (vector (unsigned-byte 8)) head))
     (declare (integer len))
     (let ((headers (cl-scgi:parse-headers (babel:octets-to-string head)))
           (body (cl-scgi:read-until-content-length len stream)))
       ;; print headers as a list
       (print (alexandria:hash-table-plist headers))
       (print (babel:octets-to-string body))
       (cl-scgi:response-string (make-hash-table) "asd" stream)
       (force-output stream)))

(unix-server "/unix.sock" #'callback :backlog 14 :multi-threaded t)
(tcp-server "127.0.0.1" 6970 #'callback :backlog 24 :multi-threaded nil)
```

Though, quite simple, it is not powerful enough for most use-cases. That is because `cl-scgi` isn't intended to be a framework or anything superfluous but simply an implementation of a good protocol. The developer decides how they'd like to structure their code.

## helper functions
Both `unix-sockets` and `usocket`, the underlying socket libraries, do not provide `write-sequence` methods. Meaning that you have to write each byte slowly instead of passing in a vector unless you use `cl-scgi:write-bytes`.

`cl-scgi:write-bytes` is essentially a wrapper around `write-sequence` without regard to :start and :end keys.

`cl-scgi:format-headers` takes in a hash-table and a stream and writes that hash-table in HTTP header format to the stream.

`cl-scgi:response-string` is a function that takes in a hash-table, a string, and a stream and writes all of those to the stream. This function accepts an empty hash-table. Do note that it will overwrite any "Content-Length" parameters with the actual content of the body.

`cl-scgi:read-until-content-length` is a function that takes in a number and a stream, and reads the stream until that number is reached. `read-until-content-length` is used to read the body of a request because most callbacks receive the header bytes, and the content-length.

`cl-scgi:read-until-eof` is a function that reads until a stream return EOF. Do note that if you use this `tcp-server` or `unix-server` that it will hang until the web-server passes the timeout duration.

---

## feature checklist
- [x] Parsing Messages (with unit tests)
- [x] ASDF System definition
- [x] Unix Server
- [x] TCP Server
- [x] Better error handling through condition messages
- [x] Multi-threaded functionality
- [ ] Switch to fast-io for most operations
