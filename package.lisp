;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-35
  (:use)
  (:export
   :make-condition-type :condition-type? :make-condition :condition?
   :condition-has-type? :condition-ref :make-compound-condition
   :extract-condition :define-condition-type :condition :&condition :&message
   :&serious :&error :message-condition? :condition-message :serious-condition?
   :error? ))

(defpackage :srfi-35.internal
  (:use :srfi-35 :cl :fiveam :srfi-23 :srfi-9 :mbe)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :lambda :member :assoc :map :loop
           :make-condition
           :condition))

;;; eof
