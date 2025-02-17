(in-package :aoc-problems)

(defun aoc08 ()
  (let ((silver 0)
        (gold 0))
    (aoc-utils:dolines
      line *standard-input*
      (let ((literal-length (length line))
            (i 0)
            (mem-length 0)
            (encoded-length 2))
        (loop
          (unless (< i (length line)) (return))
          (cond
            ((char= (aref line i) #\")
             (incf i)
             (incf encoded-length 2))
            ((and (> (- (length line) i) 3) (string= (subseq line i (+ i 2)) "\\x"))
             (incf i 4)
             (incf mem-length)
             (incf encoded-length 5))
            ((and (> (- (length line) i) 1) (char= (aref line i) #\\))
             (incf i 2)
             (incf mem-length)
             (incf encoded-length 4))
            (t
             (incf i)
             (incf mem-length)
             (incf encoded-length))))
        (incf silver (- literal-length mem-length))
        (incf gold (- encoded-length literal-length))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
