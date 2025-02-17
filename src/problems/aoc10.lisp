(in-package :aoc-problems)

(defun run-length-encode (s)
  (let ((runs nil)
        (cur nil)
        (ct 0))
    (dotimes (i (length s))
      (let ((v (aref s i)))
        (if (or (null cur) (/= cur v))
          (progn
            (unless (null cur) (push ct runs) (push cur runs))
            (setf cur v)
            (setf ct 1))
          (incf ct))))
    (unless (null cur) (push ct runs) (push cur runs))
    (nreverse runs)))

(defun aoc10 ()
  (let ((silver 0)
        (gold 0))
    (let* ((l (map 'vector #'(lambda (x) (- (char-code x) (char-code #\0))) (read-line)))
           (runs (run-length-encode l)))
      (dotimes (i 49)
        (setf runs (run-length-encode (make-array (length runs) :initial-contents runs))))
      (setf silver (length runs)))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
