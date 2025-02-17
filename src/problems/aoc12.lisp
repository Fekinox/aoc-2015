(in-package :aoc-problems)

(defun aoc12 ()
  (let ((silver 0)
        (gold 0))
    (let ((json (jsown:parse (read-line))))
      (labels
          ((reader (s red)
             (cond
               ((numberp s)
                (incf silver s)
                (unless red (incf gold s)))
               ((and (listp s) (eq (first s) :obj))
                (let ((new-red
                       (or red
                           (some #'(lambda (x) (equalp (rest x) "red")) (rest s)))))
                  (dolist (r (rest s))
                    (reader (rest r) new-red))))
               ((listp s)
                (dolist (r s)
                  (reader r red))))))
        (reader json nil)))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
