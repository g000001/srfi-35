;;;; srfi-35.lisp

(cl:in-package :srfi-35.internal)


#|(define-function (make-condition-type name supertype fields)
  (if (not (symbol? name))
      (error "make-condition-type: name is not a symbol"
             name))
  (if (not (condition-type? supertype))
      (error "make-condition-type: supertype is not a condition type"
             supertype))
  (if (not
       (null? (lset-intersection eq?
                                 (condition-type-all-fields supertype)
                                 fields)))
      (error "duplicate field name" ))
  (really-make-condition-type name
                              supertype
                              fields
                              (append (condition-type-all-fields supertype)
                                      fields)))|#

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
       (and (subtypep (class-of thing) 'cl:condition)
            (typep thing ',name) ))))

(defun condition-type? (cond)
  (subtypep cond 'cl:condition))

(defun condition-subtype? (subtype supertype)
  (subtypep subtype supertype))

#|(define (condition-type-field-supertype condition-type field)
  (let loop ((condition-type condition-type))
    (cond ((not condition-type) #f)
          ((memq field (condition-type-fields condition-type))
           condition-type)
          (else
           (loop (condition-type-supertype condition-type))))))|#

; The type-field-alist is of the form
; ((<type> (<field-name> . <value>) ***) ***)

(define-function make-condition #'cl:make-condition)

#|(define (condition-has-type? condition type)
  (any (lambda (has-type)
         (condition-subtype? has-type type))
       (condition-types condition)))|#

(defun condition-ref (condition field)
  (slot-value condition field))

#|(define (type-field-alist-ref type-field-alist field)
  (let loop ((type-field-alist type-field-alist))
    (cond ((null? type-field-alist)
           (error "type-field-alist-ref: field not found"
                  type-field-alist field))
          ((assq field (cdr (car type-field-alist)))
           => cdr)
          (else
           (loop (cdr type-field-alist))))))|#

#|
 (define (make-compound-condition condition-1 . conditions)
  (really-make-condition
   (apply append (map condition-type-field-alist
                      (cons condition-1 conditions)))))|#

;;; FIXME
(defun make-compound-condition (&rest conditions)
  (let ((name (gensym)))
    (eval
     `(progn
        (define-condition ,name (,@conditions) () )))
    (find-class name)))

#|(define (extract-condition condition type)
  (let ((entry (find (lambda (entry)
                              (condition-subtype? (car entry) type))
                            (condition-type-field-alist condition))))
    (if (not entry)
        (error "extract-condition: invalid condition type"
                      condition type))
    (really-make-condition
      (list (cons type
                  (map (lambda (field)
                         (assq field (cdr entry)))
                       (condition-type-all-fields type)))))))|#

#|(define-syntax condition
  (syntax-rules ()
    ((condition (?type1 (?field1 ?value1) ***) ***)
     (type-field-alist->condition
      (list
       (cons ?type1
             (list (cons '?field1 ?value1) ***))
       ***)))))|#

#|(define (type-field-alist->condition type-field-alist)
  (really-make-condition
   (map (lambda (entry)
          (cons (car entry)
                (map (lambda (field)
                       (or (assq field (cdr entry))
                           (cons field
                                 (type-field-alist-ref type-field-alist field))))
                     (condition-type-all-fields (car entry)))))
        type-field-alist)))|#

#|(define (condition-types condition)
  (map car (condition-type-field-alist condition)))|#

#|(define (check-condition-type-field-alist the-type-field-alist)
  (let loop ((type-field-alist the-type-field-alist))
    (if (not (null? type-field-alist))
        (let* ((entry (car type-field-alist))
               (type (car entry))
               (field-alist (cdr entry))
               (fields (map car field-alist))
               (all-fields (condition-type-all-fields type)))
          (for-each (lambda (missing-field)
                      (let ((supertype
                             (condition-type-field-supertype type missing-field)))
                        (if (not
                             (any (lambda (entry)
                                    (let ((type (car entry)))
                                      (condition-subtype? type supertype)))
                                  the-type-field-alist))
                            (error "missing field in condition construction"
                                   type
                                   missing-field))))
                    (lset-difference eq? all-fields fields))
          (loop (cdr type-field-alist))))))|#

(define-condition-type &condition cl:condition condition?)

(define-condition-type &message &condition
  message-condition?
  (message condition-message))

(define-condition-type &serious cl:serious-condition
  serious-condition?)

(define-condition-type &error &serious
  error?)

;;; eof
