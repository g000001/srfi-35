(cl:in-package :srfi-35.internal)
;; (in-readtable :srfi-35)

(def-suite srfi-35)

(in-suite srfi-35)

(define-condition-type &test-c &condition test-c?)

(test make-condition
  (is-true (test-c? (make-condition &c))))


;;; eof
