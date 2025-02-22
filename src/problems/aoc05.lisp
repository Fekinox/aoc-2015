(in-package :aoc-problems)

(defun naughty-or-nice-silver (s)
  (let ((bad-sequences (list "ab" "cd" "pq" "xy"))
        (vowels "aeiou")
        (num-vowels 0)
        (consecutive-letters nil))
    (dotimes (i (length s))
      (when (find (aref s i) vowels) (incf num-vowels))
      (when (< i (- (length s) 1))
        (let ((pair (subseq s i (+ i 2))))
          (when (char= (aref pair 0) (aref pair 1)) (setf consecutive-letters t))
          (when (find-if #'(lambda (n) (string= pair n)) bad-sequences)
            (return-from naughty-or-nice-silver nil)))))
    (and (>= num-vowels 3) consecutive-letters)))

(defun naughty-or-nice-gold (s)
  (let ((pairs (make-hash-table :test #'equal))
        (non-overlapping-pair nil)
        (repeated-letter-with-space nil))
    (dotimes (i (length s))
      (unless (or repeated-letter-with-space (>= i (- (length s) 2)))
        (when (and (char= (aref s i) (aref s (+ i 2))))
          (setf repeated-letter-with-space t)))
      (unless (or non-overlapping-pair (>= i (- (length s) 1)))
        (let* ((pair (subseq s i (+ i 2)))
               (prev-pair-index (gethash pair pairs)))
          (when (and prev-pair-index (> (- i prev-pair-index) 1))
            (setf non-overlapping-pair t))
          (unless prev-pair-index (setf (gethash pair pairs) i)))))
    (and non-overlapping-pair repeated-letter-with-space)))

(defun aoc05 ()
  (let ((silver 0)
        (gold 0))
    (aoc-utils:dolines line *standard-input*
      (when (naughty-or-nice-silver line) (incf silver))
      (when (naughty-or-nice-gold line) (format t "~a nice~%" line) (incf gold)))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
