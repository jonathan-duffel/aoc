#!/opt/homebrew/bin/sbcl --script
; Jonathan Balls AOC Day 2
; https://adventofcode.com/2022/day/1
(load "~/.quicklisp/setup.lisp")
(ql:quickload :str)
(ql:quickload :cl-utilities)

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defparameter lines (str:lines (file-get-contents "input.txt")))


(defun participation-score (pair)
    (cond
    ((equal (second pair) 'R) 1)
    ((equal (second pair) 'P) 2)
    ((equal (second pair) 'S) 3)
    ))

(defun result-score (pair)
    (cond
    ((equal pair '(R S)) 0)
    ((equal pair '(S P)) 0)
    ((equal pair '(P R)) 0)
    ((equal (first pair) (second pair)) 3)
    (T 6)
    ))

(defun round-score (pair) (+ (participation-score pair) (result-score pair)))

(defun parse-choice (c)
    (cond
    ((member c '(#\A #\X)) 'R)
    ((member c '(#\B #\Y)) 'P)
    ((member c '(#\C #\Z)) 'S)
    )
)

(defun parse-round (s) (
    list (parse-choice (char s 0)) (parse-choice (char s 2))
))

; Part 1
(print (apply '+ (mapcar #'round-score (mapcar #'parse-round lines))))

; Part 2
; X -> lose
; Y -> draw
; Z -> win

; Returns what to play against c to win
(defun winner (c)
    (cond
    ((equal c 'R) 'P)
    ((equal c 'P) 'S)
    ((equal c 'S) 'R)
))

; Returns what to play against c to lose
(defun loser (c)
    (cond
    ((equal c 'P) 'R)
    ((equal c 'R) 'S)
    ((equal c 'S) 'P)
))

(defun translate-round (line) 
    (let
    ((opp (parse-choice (char line 0))))
    (cond
        ((equal (char line 2) #\Y) (list opp opp))
        ((equal (char line 2) #\X) (list opp (loser opp)))
        ((equal (char line 2) #\Z) (list opp (winner opp)))
    )
    )
)

(print (apply '+ (mapcar #'round-score (mapcar #'translate-round lines))))
