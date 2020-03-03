;;;; srfi-35.lisp

(cl:in-package "https://github.com/g000001/srfi-35#internals")


(defmacro define-condition-type (name supertype predicate &body fields)
  `(progn
     (cl:define-condition ,name (,supertype)
       ,(mapcar (lambda (x)
                  (destructuring-bind (?field1 ?accessor1)
                                      x
                    `(,?field1 :accessor ,?accessor1
                               :initarg ,(intern (string ?field1)
                                                 :keyword))))
                fields ))
     (defparameter ,name (find-class ',name))
     (defun ,predicate (thing)
       (subtypep (class-of thing) ',name))))


(defun condition-type? (cond)
  (subtypep cond 'cl:condition))


(defun condition-subtype? (subtype supertype)
  (subtypep subtype supertype))


; The type-field-alist is of the form
; ((<type> (<field-name> . <value>) ***) ***)

(defun make-condition (type &rest args)
  (cond ((eq type &message)
         (apply #'cl:make-condition
                (quote &message)
                :format-control "~A"
                :format-arguments (list (getf args :message))
                args))
        (T (apply #'cl:make-condition type args) )))


(defun condition-has-type? (condition type)
  (declare (ignore condition type))
  'T)


(defun condition-ref (condition field)
  (slot-value condition field))


(defun get-cond-slots (conditions)
  (mapcar (lambda (c)
            (cons c (mapcar #'c2mop:slot-definition-name
                            (c2mop:class-slots (class-of c)))))
          conditions))


(defun ensure-cond-slots (conditions)
  (remove-duplicates
   (mapcan (lambda (x)
             (mapcar (lambda (y)
                       `(,y ,@(when (cl:slot-boundp (car x) y)
                                `(:initform (quote ,(slot-value (car x) y))))) ) ;already evaled
                     (cdr x) ))
           (get-cond-slots conditions) )
   :key #'car
   :from-end T))


(defun make-compound-condition (&rest conditions)
  (let ((name (gensym "ANONYMOUS-COMPOUND-CONDITION-")))
    (eval
     `(progn
        (define-condition ,name (,@(mapcar #'type-of conditions))
          (,@(ensure-cond-slots conditions)) )))
    (make-condition name)))


(defmacro condition (&rest types-fields)
  (let ((names (mapcar #'car types-fields))
        (fields (mapcan (lambda (x)
                          (mapcar (lambda (f)
                                    `(,(nth 0 f) :initform ,(nth 1 f)))
                                  (cdr x)))
                        types-fields))
        (tem (gensym "ANONYMOUS-CONDITION-")))
    `(eval ; FIXME (SBCL: attempt to dump reference to obsolete class)
      '(progn
        (define-condition ,tem (,@names)
          (,@fields))
        (make-condition ',tem)))))


(define-condition-type &condition cl:condition condition?)


(define-condition-type &foo &condition foocond?)


(define-condition-type &message cl:simple-condition
  message-condition?
  (message condition-message))


(define-condition-type &serious &condition
  serious-condition?)


(define-condition-type &error &serious
  error?)


;;; *EOF*


