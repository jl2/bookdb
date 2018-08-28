;;;; bookdb.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:bookdb)

(defmacro with-gui-thread (&body body)
  "Wraps BODY in code which masks float traps.
   This is needed in SBCL on OSX because native code often
   generate :inexact traps and land you in the debugger.
   For non SBCL this wraps body in a progn."
  `(trivial-main-thread:call-in-main-thread
    (lambda () 
      #+sbcl (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
               ,@body)
      #+ccl (unwind-protect (progn
                              (ccl:set-fpu-mode :invalid nil)
                              ,@body)
              (ccl:set-fpu-mode :invalid t))
      #-(or sbcl ccl)
      ,@body)))

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
  
(defun read-bar-code (img display &key (threshold 100))
  (let* ((size (cv:get-size img))
         (width (cv:size-width size))
         (height (cv:size-height size))
         (bars nil))
    (handler-case
        (loop
           with bar-start = nil
           for j below width do
             (let ((y-coord (floor (/ height 3))))
               (cond ((and (< (cv:get-real-2d img y-coord j) threshold)
                           (null bar-start))
                      (setf bar-start j))
                     ((and bar-start
                           (> (cv:get-real-2d img y-coord j) threshold))
                      (push (cons j bar-start) bars)
                      (loop for xl from bar-start to j do
                           (cv:set-2d display y-coord xl (cv:scalar 0 0 255)))
                      (setf bar-start nil)))))
      (error (err) (format t "Error: ~a~%" err)))
    (when bars (format t "Found bars: ~a~%" bars))
    bars))

(defun bar-widths-to-numbers (gaps)
  (let ((min-max (loop for gap in gaps minimize (- (car gap) (cdr gap)) into min
                    maximize (- (car gap) (cdr gap)) into max
                    finally (return (cons min max))))
        (number-mapping #( #(3 2 1 1) ;; 0
                          #(2 2 2 1) ;; 1
                          #(2 1 2 2) ;; 2
                          #(1 4 1 1) ;; 3
                          #(1 1 3 2) ;; 4
                          #(1 2 3 1) ;; 5
                          #(1 1 1 4) ;; 6
                          #(1 3 1 2) ;; 7
                          #(1 2 1 3) ;; 8
                          #(3 1 1 2))))
    (format t "min-max: ~a~%" min-max)))

(defun process-images (input temp output)
  (let ((threshold 90))
    ;;(cv:normalize input temp )
    (cv:adaptive-threshold  input temp 240 cv:+adaptive-thresh-mean-c+ cv:+thresh-binary+ 3 7)
      (cv:threshold input temp threshold 255 cv:+thresh-binary+)
    (bar-widths-to-numbers (read-bar-code temp output :threshold threshold))
     ;;(cv:cvt-color temp output cv:+gray-2-rgb+)
    ))

(defun scan-barcodes (&key (camera 0) (fps 60))
  (with-gui-thread
    (cv:with-named-window ("bar-code-scanner")
      (cv:with-captured-camera (vid :width 1920 :height 1080
                                    :idx camera)
        (loop
           (let* ((frame (cv:query-frame vid)))
             (cv:with-ipl-images ((src (cv:get-size frame) cv:+ipl-depth-8u+ 1)
                                  (dst (cv:get-size frame) cv:+ipl-depth-8u+ 1))
               (cv:cvt-color frame src cv:+rgb-2-gray+)
               (process-images src dst frame)
               (cv:show-image "bar-code-scanner" frame))
             (let ((c (cv:wait-key (floor (/ 1000 fps)))))
               (format t "Got: ~a~%" c)
               (when (or (= c 27) (= c 1048603))
                 (format t "Exiting~%")
                 (return)))))))))
