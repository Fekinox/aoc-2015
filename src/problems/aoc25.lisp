(in-package :aoc-problems)

; (1, x): T_1, T_2, T_3, T_4, ... T_n
; (2, x): T_1 + 1, T_2 + 2, T_3 + 3, ... T_n + n
; (3, x): T_1 + 1 + 2, T_2 + 2 + 3, ... T_n + n + (n+1)
; ...
; (y, x): T_x + x + (x+1) + ... + (x+y-2)
;         1 + ... + x + x + ... + (x+y-2)
;         T_(x+y-2) + x

(defun triangular-number (n)
  (truncate (* n (1+ n)) 2))

(defun aoc25 ()
  (let ((silver 0)
        (gold nil))
    (let* ((x 3019)
           (y 3010)
           (idx (1- (+ x (triangular-number (+ x (- y 2))))))
           (code 20151125)
           (fac 252533)
           (y 1)
           (modulus 33554393))
      (format t "~a~%" idx)
      (if (= idx 0)
          (setf silver code)
          (loop
            (when (<= idx 1)
              (setf silver (mod (* code fac y) modulus))
              (return))
            (unless (zerop (mod idx 2))
              (setf y (mod (* fac y) modulus)))
            (setf fac (mod (* fac fac) modulus))
            (setf idx (truncate idx 2)))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
