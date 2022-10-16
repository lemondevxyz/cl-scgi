(defsystem "cl-scgi"
  :depends-on (#:babel
            #:str)
  :components ((:module "src"
                :components ((:file "main"))))
  :in-order-to ((test-op (test-op "cl-scgi/tests"))))

(defsystem "cl-scgi/tests"
  :depends-on ("cl-scgi" "fiveam")
  :components ((:module "t" :components ((:file "main"))))
  :perform (test-op (o c) (symbol call :fiveam '#:run! :cl-scgi)))
