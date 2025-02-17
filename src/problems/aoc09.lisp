(in-package :aoc-problems)

(defun aoc09 ()
  (let ((silver 0)
        (gold 0))
    (let (
          (locations (make-hash-table :test #'equal))
          (distances (make-hash-table :test #'equal))
          (memo-shortest (make-hash-table :test #'equal))
          (memo-longest (make-hash-table :test #'equal)))
      (aoc-utils:dolines
        line *standard-input*
        (let* ((tokens (str:words line))
               (a
                (let ((v (gethash (nth 0 tokens) locations)))
                  (if v v (setf (gethash (nth 0 tokens) locations) (hash-table-count locations)))))
               (b
                (let ((v (gethash (nth 2 tokens) locations)))
                  (if v v (setf (gethash (nth 2 tokens) locations) (hash-table-count locations)))))
               (dist (parse-integer (nth 4 tokens))))
          (setf (gethash (list a b) distances) dist)
          (setf (gethash (list b a) distances) dist)))
      (labels
          ((runner-shortest (vis cur)
             (let ((v (gethash (list vis cur) memo-shortest)))
               (when v (return-from runner-shortest v)))
             (let ((moves 0)
                   (best most-positive-fixnum)
                   (v nil))
               (dotimes (i (hash-table-count locations))
                 (unless (= (bit vis i) 1) 
                   (let ((nbs (make-array (hash-table-count locations) :element-type 'bit))
                       (dist nil))
                       (setf (bit nbs i) 1)
                       (setf dist (runner-shortest (bit-ior vis nbs) i))
                       (unless (= dist -1)
                         (incf moves)
                         (setf best (min best (+ (gethash (list cur i) distances) dist)))))))
               (if (zerop moves) (setf v 0) (setf v best))
               (setf (gethash (list vis cur) memo-shortest) v)))
           (runner-longest (vis cur)
             (let ((v (gethash (list vis cur) memo-longest)))
               (when v (return-from runner-longest v)))
             (let ((moves 0)
                   (best most-negative-fixnum)
                   (v nil))
               (dotimes (i (hash-table-count locations))
                 (unless (= (bit vis i) 1) 
                   (let ((nbs (make-array (hash-table-count locations) :element-type 'bit))
                       (dist nil))
                       (setf (bit nbs i) 1)
                       (setf dist (runner-longest (bit-ior vis nbs) i))
                       (unless (= dist -1)
                         (incf moves)
                         (setf best (max best (+ (gethash (list cur i) distances) dist)))))))
               (if (zerop moves) (setf v 0) (setf v best))
               (setf (gethash (list vis cur) memo-longest) v))))
        (let ((shortest most-positive-fixnum)
              (longest most-negative-fixnum))
          (dotimes (i (hash-table-count locations))
            (let ((nbs (make-array (hash-table-count locations) :element-type 'bit)))
              (setf (bit nbs i) 1)
              (setf shortest (min shortest (runner-shortest nbs i))
                    longest (max longest (runner-longest nbs i)))))
          (setf silver shortest
                gold longest))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
