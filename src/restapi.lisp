(in-package :cl-user)
(defpackage restapi
  (:use :cl)
  (:export :restapi
           :get-token
           :reset-token
           :call-api
           :guess-file-name
           :plist-property-not-found
           :egetf))
(in-package :restapi)

(define-condition plist-property-not-found (error)
  ((indicator :initarg :indicator :reader indicator))
  (:report (lambda (condition stream)
             (format stream "Plist property ~S not found or nil."
                     (indicator condition)))))

(defun throw-not-found-error (indicator)
  (error 'plist-property-not-found :indicator indicator))

(defun egetf (plist indicator &key (default #'throw-not-found-error))
  (cond ((getf plist indicator))
        ((functionp default) (funcall default indicator))
        (t default)))

(defun token-type (token)
  (jo:object-value token "token_type"))

(defun token-id (token)
  (or (jo:object-value token "id_token")
      (jo:object-value token "access_token"))) ; for client_credentials type

(defun authorization-header (token)
  ;; e.g., "Bearer xxx"
  (concatenate 'string (token-type token) " " (token-id token)))

(defun plist-to-query (plist)
  ;; (:item-id "xxx" :status "new") => (("itemId" . "xxx") ("status" . "new"))
  (mapcar (lambda (cons)
            (cons (jo:camel-case (symbol-name (car cons)))
                  (cdr cons)))
          (alexandria:plist-alist plist)))

(defun ensure-query (query)
  (cond ((and (listp query) (consp (car query)))
         query)
        ((and (listp query) (symbolp (car query)))
         (plist-to-query query))
        (t (error "invalid parameter type of query ~S" query))))

(defun parse-json (body)
  (jo:parse body))

(defun %restapi (method server &rest args
                 &key path query token (parser 'parse-json) debug-restapi
                   (content-type "application/json")
                 &allow-other-keys)
  (remf args :content-type)
  (when (stringp path)
    (setf path (list path)))
  (let* ((uri (quri:uri (apply #'concatenate 'string server path)))
         (header (nconc (when content-type
                          (list (cons :content-type content-type)))
                        (list (cons :accept "application/json"))
                        (when token
                          (list (cons :authorization
                                      (authorization-header token)))))))
    (when query
      (setf (quri:uri-query-params uri) (ensure-query query)))
    (if debug-restapi
        (log:debug "sending request to ~A; args: ~S" uri args)
        (log:debug "sending request to ~A" uri))
    (multiple-value-bind (body status response-headers uri)
        (apply #'dex:request uri :method method :headers header :allow-other-keys t args)
      (when debug-restapi
        (log:debug "response-headers => ~S"
                   (alexandria:hash-table-alist response-headers)))
      (let ((content-type (gethash "content-type" response-headers)))
        (unless (ppcre:scan "^application/json($|;)" content-type)
          (log:warn "The response conntent-type is not \"application/json\" but ~S"
                    content-type)))
      (if debug-restapi
          (log:debug "response from ~A: status: ~S; body: ~S" uri status
                     (if (stringp body) body (type-of body)))
          (log:debug "response from ~A: status: ~S" uri status))
      (values (funcall parser body) status response-headers uri))))

;; for debug:
;; (eboshi.mcp::foma-items-get "M3C2B499F4" :parser 'identity)

(defun restapi (method server &rest args &key path token query (retries 0) (sleep 3)
                &allow-other-keys)
  (declare (type string server))
  (declare (ignore path token query))
  (log:debug "retries => ~A" retries)
  (remf args :retries)
  (remf args :sleep)
  (let ((retry-request (dex:retry-request retries :interval sleep)))
    (handler-bind
;;         ((dex:http-request-internal-server-error
;;         ((dex:http-request-bad-request #'dex:ignore-and-continue)
;;         ((dex:http-request-bad-request
;;         ((dex:http-request-failed retry-request))
        ((dex:http-request-failed
           (lambda (err)
             (log:debug "http-request-failed: ~A" err)
             (funcall retry-request err)
             (if (> retries 0)
                 (log:debug "retried ~A times, but all the retries failed"
                            retries)
                 (log:debug "no retries, giving up")))))
;;       (apply #'%restapi method server path args))))
;;       (apply #'%restapi method server :path path :query query :token token
; ;                                      :allow-other-keys t args))))
;;       (apply #'%restapi method server :allow-other-keys t args))))
      (apply #'%restapi method server args))))
;;       (apply #'%restapi method server :path path :token token args))))



(defparameter *prefetch-seconds* (* 60 60))

(defvar *token-cache* (make-hash-table))

(defun now ()
  (- (get-universal-time) 2208988800))

(defun expired (token)
  (let ((token-id (token-id token)))
    (if token-id
        (let* ((claims (jose:inspect-token token-id))
               (exp (assoc "exp" claims :test #'string=)))
          (when exp
            (or (not (integerp (cdr exp)))
                (< (- (cdr exp) *prefetch-seconds*) (now)))))
        t)))

(defun build-token-request (config)
  (let* ((grant-type (egetf config :grant-type :default "password"))
         (json (ecase (intern grant-type :keyword)
                 (:|password|
                  `(("grant_type" . ,grant-type)
                    ("client_id" . ,(egetf config :client-id))
                    ("username" . ,(egetf config :username))
                    ("password" . ,(egetf config :password))
                    ("connection" . ,(egetf config :connection))
                    ("scope" . ,(egetf config :scope :default ""))))
                 (:|client_credentials|
                  `(("grant_type" . ,grant-type)
                    ("client_id" . ,(egetf config :client-id))
                    ("client_secret" . ,(egetf config :client-secret))
                    ("audience" . ,(egetf config :audience))
;;                     ("scope" . ,(egetf config :scope :default ""))
                    ))
                 (:|http://auth0.com/oauth/grant-type/password-realm|
                  `(("grant_type" . ,grant-type)
                    ("username" . ,(egetf config :username))
                    ("password" . ,(egetf config :password))
                    ("audience" . ,(egetf config :audience))
                    ("client_id" . ,(egetf config :client-id))
                    ("realm" . ,(egetf config :realm))
                    ("scope" . ,(egetf config :scope :default "")))))))
    (jojo:to-json json :from :alist)))

(defun %get-token (config &key debug-restapi)
  (let* ((url (egetf config :auth0-url))
         (body (build-token-request config)))
    (restapi :POST url :content body :debug-restapi debug-restapi :retries 2)))

(defun get-token (service config &key debug-restapi try-fetch)
  (when (getf config :no-auth-required)
    (return-from get-token nil))
  (let* ((token (gethash service *token-cache*)))
    (when (or (expired token) try-fetch)
      (handler-bind ((error (lambda (err)
                              (log:debug "get-token: err => ~A" err)
                              (if token (return-from get-token token)))))
        (setf token (%get-token config :debug-restapi debug-restapi))
        (setf (gethash service *token-cache*) token)))
    token))

(defun reset-token (&optional service)
  (if service
      (setf (gethash service *token-cache*) nil)
      (clrhash *token-cache*)))

(defun call-api (service method url &rest args &key path config debug-restapi
                 &allow-other-keys)
  (declare (ignorable path))
  (let ((token (get-token service config :debug-restapi debug-restapi)))
    (apply #'restapi method url :token token args)))



;;;; Helper functions

(defun guess-file-name (content-disposition)
  (multiple-value-bind (match regs)
      (ppcre:scan-to-strings "filename=([^;]+);" content-disposition)
    (when match (aref regs 0))))
