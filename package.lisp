;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:bookdb
  (:use #:cl #:alexandria #:st-json)
  (:export #:check-isbn
           #:isbn-checksum
           #:isbn-10-to-13
           #:open-database
           #:close-database
           #:with-db
           #:create-book-database
           #:clear-book-database
           #:lookup-isbn
           #:lookup-isbns
           #:book
           #:create-book
           #:create-book-from-json
           #:add-book
           #:add-isbn
           #:find-book
           ))
