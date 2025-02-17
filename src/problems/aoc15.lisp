(in-package :aoc-problems)

(defun aoc15 ()
  (let ((silver 0)
        (gold 0))
    (let ((memo-silver (make-hash-table :test #'equal))
          (memo-gold (make-hash-table :test #'equal))
          (ingredients nil))
      (aoc-utils:dolines
        line *standard-input*
        (let ((tokens (str:words line)))
          (push (mapcar
                  #'(lambda (i) (parse-integer (nth i tokens) :junk-allowed t))
                  (list 2 4 6 8 10))
                ingredients)))
      (format t "~a~%" ingredients)
      (labels
          ((runner-silver (ingredient-counts)
             (when (gethash ingredient-counts memo-silver)
               (return-from runner-silver (gethash ingredient-counts memo-silver)))
             (let ((v 0))
               (let* ((cur-sum (reduce #'+ ingredient-counts))
                      (real-counts (cons (- 100 cur-sum) ingredient-counts))
                      (a (format t "~a~%" real-counts))
                      (scaled-counts
                       (mapcar
                         #'(lambda (ic in)
                             (mapcar #'(lambda (x) (* x ic)) in))
                         real-counts ingredients))
                      (summed-counts
                       (apply #'mapcar
                              (cons #'+ scaled-counts)))
                      (filtered-counts
                       (mapcar
                         #'(lambda (x) (max x 0))
                         (butlast summed-counts)))
                      (total-calories (nth 5 summed-counts)))
                 (setf v (reduce #'* filtered-counts)))

               (unless (>= (reduce #'+ ingredient-counts) 100)
                 (dotimes (i (1- (length ingredients)))
                   (let ((new-ic (copy-list ingredient-counts)))
                     (incf (nth i new-ic))
                     (setf v (max v (runner-silver new-ic))))))
               (setf (gethash ingredient-counts memo-silver) v)))

           (runner-gold (ingredient-counts)
             (when (gethash ingredient-counts memo-gold)
               (return-from runner-gold (gethash ingredient-counts memo-gold)))
             (let ((v 0))
               (let* ((cur-sum (reduce #'+ ingredient-counts))
                      (real-counts (cons (- 100 cur-sum) ingredient-counts))
                      (a (format t "~a~%" real-counts))
                      (scaled-counts
                       (mapcar
                         #'(lambda (ic in)
                             (mapcar #'(lambda (x) (* x ic)) in))
                         real-counts ingredients))
                      (summed-counts
                       (apply #'mapcar
                              (cons #'+ scaled-counts)))
                      (filtered-counts
                       (mapcar
                         #'(lambda (x) (max x 0))
                         (butlast summed-counts)))
                      (total-calories (nth 4 summed-counts)))
                 (setf v
                       (if (= total-calories 500)
                           (reduce #'* filtered-counts)
                           most-negative-fixnum)))

               (unless (>= (reduce #'+ ingredient-counts) 100)
                 (dotimes (i (1- (length ingredients)))
                   (let ((new-ic (copy-list ingredient-counts)))
                     (incf (nth i new-ic))
                     (setf v (max v (runner-gold new-ic))))))
               (setf (gethash ingredient-counts memo-gold) v))))
        (setf silver (runner-silver (mapcar (constantly 0) (butlast ingredients))))
        (setf gold (runner-gold (mapcar (constantly 0) (butlast ingredients))))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
