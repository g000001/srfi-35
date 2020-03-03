;;;; package.lisp

(cl:in-package cl-user)


(defpackage "https://github.com/g000001/srfi-35"
  (:use)
  (:export
   make-condition-type condition-type? make-condition condition?
   condition-has-type? condition-ref make-compound-condition
   extract-condition define-condition-type condition &condition &message
   &serious &error message-condition? condition-message serious-condition?
   error? ))


(defpackage "https://github.com/g000001/srfi-35#internals"
  (:use 
   "https://github.com/g000001/srfi-35"
   "https://github.com/g000001/srfi-23"
   "https://github.com/g000001/srfi-9"
    cl
    fiveam
    mbe)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-23" error)
  (:shadow lambda member assoc map loop)
  (:shadowing-import-from 
   "https://github.com/g000001/srfi-35"
   make-condition
   condition))


;;; *EOF*
