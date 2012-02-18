(cl:in-package :srfi-35.internal)
;; (in-readtable :srfi-35)

(def-suite srfi-35)

(in-suite srfi-35)

(define-condition-type &test-c &condition test-c?)

(define-condition-type &c &condition
  c?
  (x c-x))

(define-condition-type &c1 &c
  c1?
  (a c1-a))

(define-condition-type &c2 &c
  c2?
  (b c2-b))

(defvar v1 (make-condition &c1 :x "V1" :a "a1"))

(defvar v2 (condition (&c2 (x "V2") (b "b2"))))
(defvar v3 (condition (&c1 (x "V3/1") (a "a3"))
                      (&c2 (b "b3"))))
(defvar v4 (make-compound-condition v1 v2))
(defvar v5 (make-compound-condition v2 v3))

(test make-condition
  (is-true (test-c? (make-condition &test-c)))
  (is-true (condition-type? &test-c))
  (is-true (condition? (make-condition &test-c)))
  (is (string= ""
               (condition-ref (make-condition &message :message "")
                              'message)))
  (is-true (c? v1))
  (is-true (c1? v1))
  (is-false (c2? v1))
  (is (string= (c-x v1) "V1"))
  (is (string= (c1-a v1) "a1"))
  (is-true (c? v2))
  (is-false (c1? v2))
  (is-true (c2? v2))
  (is (string= "V2" (c-x v2)))
  (is (string= "b2" (c2-b v2)))
  (is-true (c? v3))
  (is-true (c1? v3))
  (is-true (c2? v3))
  (is (string= (c-x v3) "V3/1"))
  (is (string= (c1-a v3) "a3"))
  (is (string= (c2-b v3) "b3"))
  (is-true (c? v4))
  (is-true (c1? v4))
  (is-true (c2? v4))
  (string= (c-x v4) "V1")
  (string= (c1-a v4) "a1")
  (string= (c2-b v4) "b2")
  (is-true (c? v5))
  (is-true (c1? v5))
  (is-true (c2? v5))
  (string= (c-x v5) "V2")
  (string= (c1-a v5) "a3")
  (string= (c2-b v5) "b2"))

;;; eof
