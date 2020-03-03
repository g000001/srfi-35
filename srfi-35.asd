;;;; srfi-35.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)


(defsystem :srfi-35
  :version "20200304"
  :description "SRFI 35 for CL: Conditions"
  :long-description "SRFI 35 for CL: Conditions
https://srfi.schemers.org/srfi-35"
  :author "Richard Kelsey, Michael Sperber"
  :maintainer "CHIBA Masaomi"
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


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-35))))
  (let ((name "https://github.com/g000001/srfi-35")
        (nickname :srfi-35))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-35))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-35#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-35)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
