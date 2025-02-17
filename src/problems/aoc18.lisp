(in-package :aoc-problems)

(defun aoc18 ()
  (let ((silver 0)
        (gold 0))
    (let ((lines (make-array 0 :adjustable t :fill-pointer 0))
          (silver-grid nil)
          (gold-grid nil))
      (aoc-utils:dolines
        line *standard-input*
        (vector-push-extend line lines))
      (let* ((height (length lines))
             (width (length (aref lines 0)))
             (in-range 
              (lambda (x y)
                (and
                 (>= x 0)
                 (>= y 0)
                 (< x width)
                 (< y height)))))
        (setf silver-grid (make-array (list height width))
              gold-grid (make-array (list height width)))
        (dotimes (y height)
          (dotimes (x width)
            (setf (aref silver-grid y x)
                  (aref (aref lines y) x))
            (if
                (or
                  (and (= x 0) (= y 0))
                  (and (= x (1- width)) (= y 0))
                  (and (= x (1- width)) (= y (1- height)))
                  (and (= x 0) (= y (1- height))))
                (setf (aref gold-grid y x) #\#)
                (setf (aref gold-grid y x)
                      (aref (aref lines y) x)))))
        (labels
            ((next-step (x y grid)
               (let ((count 0)
                     (is-alive nil))
                 (loop for dx from -1 to 1 do
                       (loop for dy from -1 to 1 do
                             (let ((xx (+ x dx))
                                   (yy (+ y dy)))
                               (when (funcall in-range xx yy)
                                 (if (and (= x xx) (= y yy))
                                     (setf is-alive (char= (aref grid y x) #\#))
                                     (when (char= (aref grid yy xx) #\#)
                                       (incf count)))))))
                 (if (or
                       (and is-alive (<= count 3) (>= count 2))
                       (and (not is-alive) (= count 3)))
                     #\#
                     #\.))))
          (dotimes (i 100)
            (let ((new-silver-grid (make-array (list height width)))
                  (new-gold-grid (make-array (list height width))))
              (dotimes (y height)
                (dotimes (x width)
                  (setf (aref new-silver-grid y x) (next-step x y silver-grid))
                  (if
                      (or
                        (and (= x 0) (= y 0))
                        (and (= x (1- width)) (= y 0))
                        (and (= x (1- width)) (= y (1- height)))
                        (and (= x 0) (= y (1- height))))
                      (setf (aref new-gold-grid y x) #\#)
                      (setf (aref new-gold-grid y x)
                            (next-step x y gold-grid)))))
              (setf silver-grid new-silver-grid
                    gold-grid new-gold-grid)))

          (dotimes (y height)
            (dotimes (x width)
              (when (char= (aref silver-grid y x) #\#)
                (incf silver))
              (when (char= (aref gold-grid y x) #\#)
                (incf gold)))))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
