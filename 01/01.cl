#!/opt/homebrew/bin/sbcl --script
; Jonathan Balls AOC Day 1
; https://adventofcode.com/2022/day/1
(load "~/.quicklisp/setup.lisp")
(ql:quickload :str)
(ql:quickload :cl-utilities)

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun sum (list) (apply #'+ list))

(defun get-elf-inventories ()
  (cl-utilities:split-sequence "" (str:lines (file-get-contents "input.txt")) :test #'string=))

(defun parse-elf-inventory (inventory) (mapcar #'parse-integer inventory))

(defun inventories () (reverse (sort (mapcar #'sum (mapcar #'parse-elf-inventory (get-elf-inventories))) '<)))

(print (car (inventories)))

(print (sum (list (first (inventories)) (second (inventories)) (third (inventories)))))