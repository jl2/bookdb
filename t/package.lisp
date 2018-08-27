;;;; bookdb.test..asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage :bookdb.test
  (:use :cl
        :fiveam
        :alexandria
        :bookdb))

(in-package :bookdb.test)

(def-suite :bookdb)
(in-suite :bookdb)

(test isbn-10-checksum
      (let ((isbn "0306406152"))
        (is-true (= (isbn-checksum isbn) 2))
        (is-true (check-isbn isbn))))

(test isbn-13-checksum
      (let ((isbn "9780306406157"))
        (is-true (= (isbn-checksum isbn) 7))
        (is-true (check-isbn isbn))))

(test isbn-conversion
      (let* ((isbn10 "0306406152")
             (isbnc (isbn-10-to-13 isbn10))
             (isbn13-expected "9780306406157"))
        (is-true (string= isbnc isbn13-expected))
        (is-true (check-isbn isbnc))
        (is-true (check-isbn isbn10))
        (is-true (check-isbn isbn13-expected))))
