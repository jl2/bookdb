;;;; bookdb.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:bookdb
  :description "Describe bookdb here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:common-cv #:alexandria #:j-utils #:trivial-main-thread)
  :components ((:file "package")
               (:file "bookdb"))
  :in-order-to ((test-op (test-op :bookdb.test))))
