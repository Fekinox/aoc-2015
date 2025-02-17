(in-package :aoc-problems)

(defun aoc11 ()
  (let ((silver nil)
        (gold nil))
    (let ((password (map 'vector #'char-code (read-line))))
      (loop
        (when
            (block validator
              (let ((runs 0)
                    (pairs 0)
                    (last-pair 0)) 
                (dotimes (i (length password))
                  (when (find (code-char (aref password i)) "iol")
                    (return-from validator nil))
                  (when
                      (and (>= i 2)
                           (= 1 (- (aref password i) (aref password (- i 1))))
                           (= 1 (- (aref password (- i 1)) (aref password (- i 2)))))
                    (incf runs))
                  (when
                      (and (>= i 1)
                           (> (- i last-pair) 1)
                           (= (aref password i) (aref password (- i 1))))
                    (incf pairs)
                    (setf last-pair i)))
                (and (> runs 0) (> pairs 1))))
          (when silver
            (setf gold (map 'string #'code-char password))
            (return))
          (setf silver (map 'string #'code-char password)))
        (let ((current-index (1- (length password))))
          (loop
            (when (< current-index 0) (return))
            (cond
              ((char= (code-char (aref password current-index)) #\z)
               (setf (aref password current-index) (char-code #\a))
               (decf current-index))
              ((find (code-char (aref password current-index)) "hnk")
               (incf (aref password current-index) 2)
               (return))
              (t
               (incf (aref password current-index))
               (return))))))) 
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
