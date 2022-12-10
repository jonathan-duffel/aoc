#!/opt/homebrew/bin/sbcl --script
; Jonathan Balls AOC Day 7
; https://adventofcode.com/2022/day/7
(load "~/.quicklisp/setup.lisp")
(ql:quickload :str)
; (ql:quickload :cl-utilities)

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

; Command parsing
(defparameter raw-commands (str:split "$ " (file-get-contents "input.txt")))
(defparameter input (cdr (remove nil (mapcar #'str:lines raw-commands))))

(defun parse-ls-line (line) (
    if (string= "dir" (str:substring 0 3 line))
        ; (list (str:substring 4 nil line) 'dir)
        nil
        (let ((split (str:words line))) (list (second split) (parse-integer (car split))))
))
(defun parse-ls (ls-output)
    (cons 'ls (remove nil (mapcar #'parse-ls-line (cdr ls-output))))
)
(defun parse-cd (cd-output) (list 'cd (str:substring 3 T (car cd-output))))
(defun parse-command (command) (if (string= "ls" (car command)) (parse-ls command) (parse-cd command)))
(defun parse-input (input) (mapcar #'parse-command input))

(defparameter commands (parse-input input))

; (commands directory) -> (commands directory)
(defun build-file-tree (commands directory)
    (let* (
        (command (caar commands))
        (command-value (cdar commands))
        (commands-tail (cdr commands))
    ) (cond
        ((equal command nil) (list commands directory))
        ((equal command 'ls) (build-file-tree commands-tail command-value))
        ((equal command 'cd) (cond
            ((string= (car command-value) "..") (list commands-tail directory))
            (t (let* (
                (subdir (build-file-tree commands-tail nil))
                (commands (first subdir))
                (subdir-file-tree (second subdir))
                (directory (acons (car command-value) subdir-file-tree directory))
            )
                (build-file-tree commands directory)
            ))
        ))
    ))
)

(defparameter file-tree (cons "/" (cadr (build-file-tree commands nil))))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

; file-tree -> (size child-sizes[])
(defun calculate-directory-size (file-tree) (cond
    ; If passed a file then just return the size
    ((numberp (cadr file-tree)) (list (cadr file-tree) nil))
    (T (let* (
        (child-sizes (mapcar #'calculate-directory-size (cdr file-tree)))
        (child-total-size (apply '+ (mapcar #'car child-sizes)))
        (child-subdirs (flatten (mapcar #'cadr child-sizes)))
    )
        (list child-total-size (cons child-total-size child-subdirs))
    ))
))

(defparameter dir-sizes (calculate-directory-size file-tree))
(defparameter small-dir-sizes (remove-if-not #'(lambda (n) (<= n 100000)) (cadr dir-sizes)))
(print "PART 1")
(print (apply '+ small-dir-sizes))

(print "PART 2")
(defparameter needed-new-free-space (- (car dir-sizes) 40000000))
(print (car (remove-if-not #'(lambda (n) (>= n needed-new-free-space)) (sort (cadr dir-sizes) #'<))))
