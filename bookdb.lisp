;;;; bookdb.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:bookdb)

(defun isbn-checksum (isbn)
  (declare (type string isbn))
  (cond ((= 13 (length isbn))
         (- 10 (mod 
                (loop for i fixnum below 12
                   summing (* 
                            (if (= 0 (mod i 2))
                                1
                                3)
                            (parse-integer (subseq  isbn i (1+ i)))))
                10)))
        ((= 10 (length isbn))
         (- 11 (mod (loop for i fixnum below 9
                       summing (* 
                                (- 10 i)
                                (parse-integer (subseq  isbn i (1+ i)))))
                    11)))
        (t
         (error "~s is an invalid ISBN" isbn))))

(defun check-isbn (isbn)
  (let ((checksum (isbn-checksum isbn))
        (len (length isbn)))
    (cond ((= 13 (length isbn))
           (= (parse-integer (subseq isbn (- len 1) len))
              checksum))
          ((= 10 (length isbn))
           (= (parse-integer (subseq isbn (- len 1) len))
              checksum))
          (t nil))))

(defun isbn-10-to-13 (isbn)
  (when (/= 10 (length isbn))
    (error "~s is not a 10 digit ISBN" isbn))
  (let ((rval (concatenate 'string "978" isbn)))
    (setf (aref rval 12) (code-char (+ (char-code #\0) (isbn-checksum rval))))
    rval))

(defun open-database (file-name &key (busy-timeout))
  (sqlite:connect file-name :busy-timeout busy-timeout))

(defun close-database (db)
  (sqlite:disconnect db))

(defmacro with-db ((db &optional (file-name ":memory:")) &body body)
  `(let ((,db (open-database ,file-name)))
     (unwind-protect
          (progn
            (create-book-database ,db)
            ,@body)
       (close-database ,db))))

(defclass book ()
  ((key :initarg :key :initform 0 :type (or null fixnum))
   (title :initarg :title :initform "" :type string)
   (title_long :initarg :title_long :initform "" :type string)
   (isbn :initarg :isbn :initform "" :type string)
   (isbn13 :initarg :isbn13 :initform "" :type string)
   (dewey_decimal :initarg :dewey_decimal :initform "" :type string)
   (format :initarg :format :initform "" :type string)
   (publisher :initarg :publisher :initform "" :type string)
   (language :initarg :language :initform "" :type string)
   (date_published :initarg :date_published :initform "" :type local-time:timestamp)
   (edition :initarg :edition :initform "" :type string)
   (pages :initarg :pages :initform 0 :type fixnum)
   (dimensions :initarg :dimensions :initform "" :type string)
   (overview :initarg :overview :initform "" :type string)
   (excerpt :initarg :excerpt :initform "" :type string)
   (synopsys :initarg :synopsys :initform "" :type string)
   (authors :initarg :authors :initform nil :type list)
   (author-ids :initarg :author-ids :initform nil :type list)
   (subjects :initarg :subjects :initform nil :type list)
   (subject-ids :initarg :subject-ids :initform nil :type list))
  (:documentation "Holds all of the information about a book from isbndb.org"))

(defun create-book (&key
                      (title "")
                      (title_long "")
                      (isbn "")
                      (dewey_decimal "")
                      (format "")
                      (publisher "")
                      (language "")
                      (date_published (local-time:now))
                      (edition "")
                      (pages 0)
                      (dimensions "")
                      (overview "")
                      (excerpt "")
                      (synopsys "")
                      (authors nil)
                      (subjects nil))
  (make-instance 'book
                 :key nil
                 :title title
                 :title_long title_long
                 :isbn isbn
                 :isbn (if (= 10 (length isbn)) (isbn-10-to-13 isbn) isbn)
                 :dewey_decimal dewey_decimal
                 :format format
                 :publisher publisher
                 :language language
                 :date_published date_published
                 :edition edition
                 :pages pages
                 :dimensions dimensions
                 :overview overview
                 :excerpt excerpt
                 :synopsys synopsys
                 :authors (ensure-list authors)
                 :author-ids nil
                 :subjects (ensure-list subjects)
                 :subject-ids nil))

(defun clear-book-database (db)
  (sqlite:execute-non-query db "drop table if exists books")
  (sqlite:execute-non-query db "drop table if exists authors")
  (sqlite:execute-non-query db "drop table if exists subjects")
  (sqlite:execute-non-query db "drop table if exists book_author_map")
  (sqlite:execute-non-query db "drop table if exists book_subject_map"))
  

(defun create-book-database (db)
  (sqlite:execute-non-query db "
create table books (id integer primary key,
                    title text,
                    title_long text,
                    isbn text,
                    isbn13 text,
                    dewey_decimal text,
                    format text,
                    publisher text,
                    language text,
                    date_published text,
                    edition text,
                    pages integer,
                    dimensions text,
                    overview text,
                    excerpt text,
                    synopsys text)")
  (sqlite:execute-non-query db "
create table authors (id integer primary key,
                      name text)")

  (sqlite:execute-non-query db "
create table subjects (id integer primary key,
                       subject text)")

  (sqlite:execute-non-query db "
create table book_author_map (id integer primary key,
                      book_id integer,
                      author_id integer,
                      FOREIGN KEY(book_id) REFERENCES books(id) ON DELETE CASCADE
                      FOREIGN KEY(author_id) REFERENCES authors(id) ON DELETE SET NULL)")

  (sqlite:execute-non-query db "
create table book_subject_map (id integer primary key,
                      book_id integer,
                      subject_id integer,
                      FOREIGN KEY(book_id) REFERENCES books(id) ON DELETE CASCADE
                      FOREIGN KEY(subject_id) REFERENCES subjects(id) ON DELETE SET NULL)")

  )

(defun lookup-isbn (isbn)
  (create-book
                 :title "Common Lisp Recipes"
                 :isbn isbn
                 :authors "Bob Jones"
                 :subjects '("Lisp" "Computers science" "algorithms")))

(defun add-book (db book)
  (with-slots (key
               title
               title_long
               isbn
               isbn13
               dewey_decimal
               format
               publisher
               language
               date_published
               edition
               pages
               dimensions
               overview
               excerpt
               synopsys
               authors
               author-ids
               subjects
               subject-ids) book
    (let* ((ea-sql (format
                    nil
                    "select name, id from authors where name in (~{~a~^, ~})"
                    (loop for i below (length authors) collecting "?")))

           (existing-authors (apply
                              (curry #'sqlite:execute-to-list db ea-sql)
                              authors))
           (existing-author-ids (mapcar #'cadr existing-authors))

           (es-sql (format
                    nil
                    "select subject, id from subjects where subject in (~{~a~^, ~})"
                    (loop for i below (length subjects) collecting "?")))

           (existing-subjects (apply
                               (curry #'sqlite:execute-to-list db es-sql)
                               subjects))
           (existing-subject-ids (mapcar #'cadr existing-subjects)))

      (loop for author in authors do
           (when (not (find author existing-authors :key #'car :test #'string=))
             (sqlite:execute-non-query db "insert into authors (name) values (?)" author)
             (push (sqlite:last-insert-rowid db) existing-author-ids)))
      (setf author-ids existing-author-ids)

      (dolist (auth existing-subjects)
        (push (cadr auth) existing-subject-ids))
      (loop for subject in subjects do
           (when (not (find subject existing-subjects :key #'car :test #'string=))
             (sqlite:execute-non-query db "insert into subjects (subject) values (?)" subject)
             (push (sqlite:last-insert-rowid db) existing-subject-ids)))
      (setf subject-ids existing-subject-ids)

      (sqlite:execute-non-query db "insert into books (title,
                                                       title_long,

                                                       isbn,
                                                       isbn13,
                                                       dewey_decimal,

                                                       format,
                                                       publisher,
                                                       language,

                                                       date_published,
                                                       edition,

                                                       pages,
                                                       dimensions,

                                                       overview,
                                                       excerpt,
                                                       synopsys)
                                    values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

                                title
                                title_long

                                isbn
                                isbn13
                                dewey_decimal

                                format
                                publisher
                                language

                                (with-output-to-string (outs)
                                  (local-time:format-timestring outs date_published))

                                edition

                                pages
                                dimensions

                                overview
                                excerpt
                                synopsys)
      (setf key (sqlite:last-insert-rowid db))

      (dolist (author-id author-ids)
        (sqlite:execute-non-query db "insert into book_author_map (book_id, author_id) values (?, ?)" key author-id))

      (dolist (subject-id subject-ids)
        (sqlite:execute-non-query db "insert into book_subject_map (book_id, subject_id) values (?, ?)" key subject-id)))))

