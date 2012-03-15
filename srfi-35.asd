;;;; srfi-35.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-35
  :serial t
  :depends-on (:fiveam
               :closer-mop
               :srfi-23
               :srfi-9
               :srfi-1
               :srfi-5
               :mbe)
  :components ((:file "package")
               (:file "util")
               (:file "srfi-35")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-35))))
  (load-system :srfi-35)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-35.internal :srfi-35))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
