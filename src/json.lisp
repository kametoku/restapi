(in-package :cl-user)
(defpackage restapi.json
  (:nicknames :jo)
  (:use :cl)
  (:export :camel-case
           :parse
           :to-json
           :json-type
           :array-p
           :false-p
           :null-p
           :number-p
           :object-p
           :string-p
           :true-p
           :*default-validate*
           :*default-validate-not*
           :*default-value*
           :object-value
           :with-object-values
           :with-object-values*
           :object-keys
           :name-value-cons

           :write-key-value
           :write-slots))
(in-package :restapi.json)

(defun camel-case (string)
  ;; "foo-bar" => "fooBar"
  (let ((start (or (search "-" string) (length string))))
    (remove #\- (string-capitalize (string-downcase string) :start start))))

(defun dot-to-list (string)
  ;; "foo.foo-bar[1].blab" => ("foo" "foo-bar" "[1]" "blab")
  (ppcre:all-matches-as-strings "([^.[\\]]+|\\[[^[\\]]*\\])" (string string)))

;; (defun alist-value (alist key &key default)
;;   (or (cdr (assoc key alist :test #'string=)) default))

(defun parse (text &key (as :plist))
  (let ((jojo:*false-value* 'false)
        (jojo:*null-value* 'null)
        (jojo:*empty-object-value* '{})
;;         (jojo:*empty-array-value* '[]))
        (jojo:*empty-array-value* nil))
    (jojo:parse text :as as)))

(defun to-json (object &key (from :plist))
  (let ((jojo:*from* from))
    (jojo:to-json object)))

(defmethod jojo:%to-json ((_ (eql 'false)))
  (jojo:%to-json :false))

(defmethod jojo:%to-json ((_ (eql 'null)))
  (jojo:%to-json :null))

(defmethod jojo:%to-json ((_ (eql '{})))
  (jojo:%to-json :empty))

(defmethod jojo:%to-json ((_ (eql '[])))
  (jojo:%to-json nil))

(defun json-type (value)
  "Returns the type of JSON value as a keyword.
The retrun value could be :array, :false, :null, :number, :object,
:string, or :true.
If VALUE is not a valid, it returns nil."
  (cond ((listp value)
         (if (keywordp (car value)) :object :array))
        ((numberp value) :number)
        ((stringp value) :string)
        ((eql value t) :true)
        ((eql value 'false) :false)
        ((eql value 'null) :null)
        ((eql value '{}) :object)
        ((eql value nil) :array)
        ((eql value '[]) :array)))

(defun array-p (value)
  "Non-nil if VALUE is an array."
  (eql (json-type value) :array))

(defun false-p (value)
  "Non-nil if VALUE is false."
  (eql (json-type value) :false))

(defun null-p (value)
  "Non-nil if VALUE is null."
  (eql (json-type value) :null))

(defun number-p (value)
  "Non-nil if VALUE is a number."
  (eql (json-type value) :number))

(defun object-p (value)
  "Non-nil if VALUE is an object."
  (eql (json-type value) :object))

(defun string-p (value)
  "Non-nil if VALUE is a string."
  (eql (json-type value) :string))

(defun true-p (value)
  "Non-nil if VALUE is true."
  (eql (json-type value) :true))

(defun index (key)
  ;; "[1]" => 1, "[]" => 0, "[foo]" => "foo" nil, "[name=AbcD]" => "AbcD" :|name|,
  ;; "3" => nil, "aaa" => nil
  (if (string= key "[]")
      0
      (multiple-value-bind (match regs)
          (ppcre:scan-to-strings "\\[(?:(.*)=)?(.*)\\]" key)
        (when match
          (let ((name (aref regs 0))
                (value (aref regs 1)))
            (cond ((and (not name) (parse-integer value :junk-allowed t)))
                  (t (values value (and name (intern name :keyword))))))))))

(defun ensure-key (key)
  ;; "foo" => :|foo|, "foo-bar" => :|fooBar|, :|foo-bar| => :|foo-bar|
  (if (stringp key)
      (intern (camel-case key) :keyword)
      key))

(defun name-value-find (list name &key (name-key :|name|))
  (find-if (lambda (object)
             (string= (getf object name-key) name))
           list))

(defun my-nth (n list)
  "Ruby-like list accessor, negative index returns the element from the last."
  (when (or (>= n 0)
            (>= (setf n (+ (length list) n)) 0))
    (nth n list)))

(defun array-value (object index &key name)
  (cond ((not index) nil)
        ((eql object '[]) nil)
        ((numberp index) (my-nth index object))
        (name (name-value-find object index :name-key name))
        (t (name-value-find object index))))

(defun parse-value (value &key verbatim)
  (cond (verbatim value)
        ((member value '(false null {} [])) nil)
        (t value)))

(defun %%object-value (object key)
  (case (json-type object)
    (:object (when (listp object)
               (getf object (ensure-key key))))
    (:array (multiple-value-bind (index name)
                (index key)
              (array-value object index :name name)))))

(defun %object-value (object key-path &key verbatim)
  (unless (listp key-path)
    (setf key-path (dot-to-list key-path)))
  (loop for key in key-path
        unless (setf object (%%object-value object key))
          do (return nil))
  (parse-value object :verbatim verbatim))

(defvar *default-validate* nil)

(defvar *default-validate-not* nil)

(defun %validate-object-value (value validate)
  (cond ((functionp validate) (funcall validate value))
        ((listp validate) (member value validate :test #'equal))
        (t (equal value validate))))

(defun validate-object-value (value validate validate-not)
  (unless validate (setf validate *default-validate*))
  (unless validate-not (setf validate-not *default-validate-not*))
  (cond ((and validate validate-not)
         (error "Cannot use arguments validate and validate-not simultaneously."))
        (validate (%validate-object-value value validate))
        (validate-not (not (%validate-object-value value validate-not)))
        (t t)))

(define-condition object-value-validation-error (error)
  ((key-path :initarg :key-path :reader key-path)
   (value :initarg :value :reader value))
  (:report (lambda (condition stream)
             (format stream "No parameter or invalid value ~S for parameter ~S."
                     (value condition) (key-path condition)))))

(defvar *default-value* nil)

(defun object-value (object key-path &key (default nil supplied-p-default)
                                       validate validate-not verbatim
                                       parser)
  (unless (or default supplied-p-default)
    (setf default *default-value*))
  (let ((value (cond ((%object-value object key-path :verbatim verbatim))
                     ((functionp default) (funcall default key-path))
                     (t default))))
    (unless (validate-object-value value validate validate-not)
      (error 'object-value-validation-error :key-path key-path :value value))
    (if (and parser (not (equal value default)))
        (funcall parser value)
        value)))
    
;; (defmacro with-object-values (variable-list object &body body)
;;   "(with-object-values (variable*) object form*)
;; variable ::= (variable-name key-path) | variable"
;;   (let ((gobject (gensym)))
;;     `(let* ((,gobject ,object)
;;             ,@(mapcar
;;                 (lambda (entry)
;;                   (if (symbolp entry)
;;                       (list entry
;;                             (list 'object-value gobject (string entry)))
;;                       (list (nth 0 entry)
;;                             (apply #'list 'object-value gobject (cdr entry)))))
;;                 variable-list))
;;        ,@body)))

(defmacro %with-object-values (variable-list (&key args (default '*default-value*))
                               object &body body)
  "(with-object-values (variable*) (keywords) object form*)
variable ::= (variable-name key-path) | variable"
  (let ((gobject (gensym))
        (gdefault (gensym)))
    `(let* ((,gobject ,object)
            (,gdefault ,default)
            (*default-value* ,(if args (quote 'not-exist) gdefault))
            ,@(mapcar
                (lambda (entry)
                  (if (symbolp entry)
                      `(,entry (object-value ,gobject ,(string entry)))
                      `(,(car entry)
                        ,(apply #'list 'object-value gobject (cdr entry)))))
                variable-list)
            ,@(when args
                `((,args (nconc
                          ,@(mapcar
                             (lambda (entry)
                               (let* ((sym (if (symbolp entry) entry (nth 0 entry))))
                                 `(if (eql ,sym 'not-exist)
                                      (progn (setf ,sym ,gdefault) nil)
                                      (list ,(intern (string sym) :keyword) ,sym))))
                             variable-list))))))
       (declare (ignorable ,gdefault))
       ,@(when args `((declare (ignorable ,args))))
       ,@body)))

(defmacro with-object-values (variable-list-and-options object &body body)
  "(with-object-values (variable* option) object form*)
variable ::= (variable-name key-path [[option*]]) | variable
option ::= {:arg arg} | {:default default-value}
variable-option ::= {:default default-value} |
                    {:validate validator} |
                    {:validate-not validator} |
                    {:vervatim boolean}"
  (let* ((options (member-if #'keywordp variable-list-and-options))
         (variable-list (butlast variable-list-and-options (length options))))
    (destructuring-bind (&rest rest &key args default)
        options
      (declare (ignorable args default))
      `(%with-object-values ,variable-list ,rest ,object ,@body))))

;; (defmacro with-object-values* (binds &body body)
;;   (if (null binds)
;;       `(progn ,@body)
;;       `(with-object-values ,@(car binds)
;;            (with-object-values* ,(cdr binds) ,@body)))))
(defmacro with-object-values* (binds &body body)
  (let ((proc `(progn ,@body)))
    (mapc (lambda (bind)
            (setf proc `(with-object-values ,@bind ,proc)))
          (reverse binds))
    proc))

(defun object-keys (object)
  "Return the list of objec keys."
  (loop for (key value) on object by #'cddr
        collect key))

;; (defun object-values (object)
;;   (loop for (key value) on object by #'cddr
;;         collect valuea))

(defmacro name-value-cons (object key-name key-value)
  (let ((gkey-name (gensym))
        (gkey-value (gensym)))
    `(let ((,gkey-name ,key-name)
           (,gkey-value ,key-value))
       (with-object-values (,gkey-name ,gkey-value)
           ,object
         (cons ,gkey-name ,gkey-value)))))


(defun %write-key-value (symbol &key key (default *default-value*))
  (let ((key (or key (camel-case (symbol-name symbol)))))
    `(let ((value (or ,symbol ,default)))
       (when value
         (jojo:write-key-value ,key value)))))

(defmacro write-key-value (symbol &rest args &key key default)
  (declare (ignorable key default))
  (apply #'%write-key-value symbol args))

(defmacro write-slots (slots instance)
  "Write slot values with camel case key name."
  `(with-slots ,(mapcar (lambda (slot)
                          (if (consp slot) (car slot) slot))
                 slots)
       ,instance
     ,@(mapcar (lambda (slot)
                 (if (consp slot)
                     (apply #'%write-key-value slot)
                     (%write-key-value slot)))
               slots)))
