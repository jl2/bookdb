;;;; bookdb.test.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage bookdb.test-asd
  (:use :cl :asdf))
(in-package :bookdb.test-asd)


(defsystem bookdb.test
  :author "Jeremiah LaRocco"
  :mailto "jeremiah_larocco@fastmail.com"
  :description "Test bookdb."
  :license "ISC"
  :depends-on (:bookdb
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(every #'fiveam::TEST-PASSED-P (5am:run :bookdb))"))))
