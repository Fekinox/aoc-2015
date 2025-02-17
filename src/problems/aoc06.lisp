(in-package :aoc-problems)

(defun aoc06 ()
  (let ((silver 0)
        (gold 0))
    (let ((gold-g (make-array '(1000 1000) :initial-element 0))
          (silver-g (make-array '(1000 1000) :initial-element 0)))
      (aoc-utils:dolines 
        line *standard-input*
        (let* ((tokens (str:words line))
               (op (cond ((string= (nth 1 tokens) "on") :on)
                    ((string= (nth 1 tokens) "off") :off)
                    ((string= (nth 0 tokens) "toggle") :toggle)))
               (first-corner (nth (- (length tokens) 3) tokens))
               (second-corner (nth (- (length tokens) 1) tokens))
               (top-corner (mapcar #'parse-integer (str:split "," first-corner)))
               (bot-corner (mapcar #'parse-integer (str:split "," second-corner))))
          (destructuring-bind ((ax ay) (bx by)) (list top-corner bot-corner)
            (dotimes (dx (1+ (- bx ax)))
              (dotimes (dy (1+ (- by ay)))
                (let ((xx (+ ax dx))
                      (yy (+ ay dy)))
                  (case op
                    (:on 
                     (incf (aref gold-g xx yy))
                     (setf (aref silver-g xx yy) 1))
                    (:off
                     (setf (aref gold-g xx yy) (max 0 (1- (aref gold-g xx yy))))
                     (setf (aref silver-g xx yy) 0))
                    (:toggle
                     (incf (aref gold-g xx yy) 2)
                     (setf (aref silver-g xx yy) (- 1 (aref silver-g xx yy)))))))))))
      (dotimes (i (* 1000 1000))
        (incf silver (row-major-aref silver-g i))
        (incf gold (row-major-aref gold-g i))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
