(defsystem "restapi"
  :version "0.1.0"
  :author "Tokuya Kameshima"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-ppcre"
               "dexador"
               "jonathan"
               "jose"
               "log4cl"
               "quri")
  :components ((:module "src"
                :components
                ((:file "restapi" :depends-on ("json"))
                 (:file "json"))))
  :description "A RESTful API Client."
  :in-order-to ((test-op (test-op "restapi/tests"))))

;; (defsystem "restapi/tests"
;;   :author "Tokuya Kameshima"
;;   :license ""
;;   :depends-on ("restapi"
;;                "rove")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "restapi"))))
;;   :description "Test system for restapi"
;;   :perform (test-op (op c) (symbol-call :rove :run c)))
