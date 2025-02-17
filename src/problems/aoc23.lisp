(in-package :aoc-problems)

(defun aoc23 ()
  (let ((silver 0)
        (gold 0))
    (let ((instructions (make-array 0 :adjustable t :fill-pointer 0))
          (i 0)
          (a 1)
          (b 0)
          (parse-token
           (lambda (s)
             (cond
              ((string= s "a") :a)
              ((string= s "b") :b)))))
      (aoc-utils:dolines
        line *standard-input*
        (let ((tokens (cl-ppcre:split ",?\\s+" line)))
          (vector-push-extend
            (cond
              ((string= (first tokens) "hlf")
               (list :hlf (funcall parse-token (second tokens))))
              ((string= (first tokens) "tpl")
               (list :tpl (funcall parse-token (second tokens))))
              ((string= (first tokens) "inc")
               (list :inc (funcall parse-token (second tokens))))
              ((string= (first tokens) "jmp")
               (list :jmp (parse-integer (second tokens))))
              ((string= (first tokens) "jie")
               (list
                 :jie
                 (funcall parse-token (second tokens))
                 (parse-integer (third tokens))))
              ((string= (first tokens) "jio")
               (list
                 :jio
                 (funcall parse-token (second tokens))
                 (parse-integer (third tokens)))))
            instructions)))
      (format t "~a~%" instructions)
      (loop
        (when (or (< i 0) (>= i (length instructions)))
          (return))
        (let ((current-op (aref instructions i)))
          (case (first current-op)
            (:hlf
             (if (eq (second current-op) :a)
                 (setf a (truncate a 2))
                 (setf b (truncate b 2)))
             (incf i))
            (:tpl
             (if (eq (second current-op) :a)
                 (setf a (* a 3))
                 (setf b (* b 3)))
             (incf i))
            (:inc
             (if (eq (second current-op) :a)
                 (incf a)
                 (incf b))
             (incf i))
            (:jmp
             (incf i (second current-op)))
            (:jie
             (if (or
                  (and (eq (second current-op) :a) (zerop (mod a 2)))
                  (and (eq (second current-op) :b) (zerop (mod b 2))))
                 (incf i (third current-op))
                 (incf i)))
            (:jio
             (if (or
                  (and (eq (second current-op) :a) (= 1 a))
                  (and (eq (second current-op) :b) (= 1 b)))
                 (incf i (third current-op))
                 (incf i))))))
      (setf silver b))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
