;;;; bookdb.test..asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package :cl-user)
(defpackage :bookdb.test
  (:use :cl
        :fiveam
        :st-json
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


(test clear-database
  (with-db (db)
    (is-true (> (sqlite:execute-single db "select count(*) from sqlite_master") 0))
    (clear-book-database db)
    (is-true (zerop (sqlite:execute-single db "select count(*) from sqlite_master")))))

(test add-book
  (with-db (db)
    (add-book db (create-book
                                :title "a book"
                                :authors '("bob" "tom")
                                :subjects "Stuff"))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from books")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from authors")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from book_author_map")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from subjects")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from book_subject_map")))))

(test add-duplicate-author
  (with-db (db)
    (add-book db (create-book
                  :title "a book"
                  :authors '("bob" "tom")
                  :subjects "Stuff"))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from books")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from authors")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from book_author_map")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from subjects")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from book_subject_map")))

    (add-book db (create-book
                  :title "another book"
                  :authors '("bob" "tom")
                  :subjects "More Stuff"))

    (is-true (= 2 (sqlite:execute-single db "select count(*) from books")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from authors")))
    (is-true (= 4 (sqlite:execute-single db "select count(*) from book_author_map")) (sqlite:execute-to-list db "select * from book_author_map"))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from subjects")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from book_subject_map")))))


(test add-duplicate-subject
  (with-db (db)
    (add-book db (create-book
                  :title "a book"
                  :authors "bob"
                  :subjects "Stuff"))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from books")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from authors")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from book_author_map")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from subjects")))
    (is-true (= 1 (sqlite:execute-single db "select count(*) from book_subject_map")))

    (add-book db (create-book
                  :title "another book"
                  :authors '("bob" "tom")
                  :subjects '("Stuff" "More Stuff")))

    (is-true (= 2 (sqlite:execute-single db "select count(*) from books")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from authors")))
    (is-true (= 3 (sqlite:execute-single db "select count(*) from book_author_map")))
    (is-true (= 2 (sqlite:execute-single db "select count(*) from subjects")))
    (is-true (= 3 (sqlite:execute-single db "select count(*) from book_subject_map"))
             (format t "~%~{~a ~}~%" (sqlite:execute-to-list db "select * from book_subject_map")))))


(test isbn-lookup
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :address "127.0.0.1" :port 8182))
        (example-json "{
  \"title\": \"string\",
  \"title_long\": \"string\",
  \"isbn\": ~s,
  \"isbn13\": ~s,
  \"dewey_decimal\": \"string\",
  \"format\": \"string\",
  \"publisher\": \"string\",
  \"language\": \"string\",
  \"date_published\": \"2018-09-02T23:28:27.626Z\",
  \"edition\": \"string\",
  \"pages\": 0,
  \"dimensions\": \"string\",
  \"overview\": \"string\",
  \"excerpt\": \"string\",
  \"synopsys\": \"string\",
  \"authors\": [
    \"string\"
  ],
  \"subjects\": [
    \"string\"
  ],
  \"reviews\": [
    \"string\"
  ]
}")
        (found-api-key nil))
    (unwind-protect
         (progn
           (hunchentoot:start acceptor)
           (setf bookdb::*api-key* "test-test-test-test")
           (setf bookdb::*isbndb-url* "http://127.0.0.1:8182")
           (hunchentoot:define-easy-handler (test-lookup-book :uri "/book/1234567890") (isbn)
             (setf (hunchentoot:content-type*) "application/json")
             (setf found-api-key (cdr (assoc "X-API-KEY" (hunchentoot:headers-in*) :test #'string=)))
             (format nil example-json "1234567890" "1234567890"))
           
           (let ((book (bookdb:lookup-isbn "1234567890")))
             (format t "~a~%" book)
             (is-true (string= (slot-value book 'bookdb::title ) "string")))
           (is-true (string= found-api-key "test-test-test-test" )))
      (hunchentoot:stop acceptor))))


