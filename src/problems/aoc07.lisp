(in-package :aoc-problems)

(defun number-or-string (s)
  (let ((n (parse-integer s :junk-allowed t)))
    (if n n s)))

(defun aoc07-memo ()
  (let ((silver 0)
        (gold 0))
    (let ((gates (make-hash-table :test #'equal))
          (registers (make-hash-table :test #'equal)))
      (aoc-utils:dolines
        line *standard-input*
        (let ((tokens (str:words line)))
          (cond
            ((string= (nth 0 tokens) "NOT")
             (setf 
              (gethash (nth 3 tokens) gates)
              (list (lambda (x) (logandc2 65535 x))
               (number-or-string (nth 1 tokens)))))
            ((string= (nth 1 tokens) "AND")
             (setf
               (gethash (nth 4 tokens) gates)
               (list #'logand
                (number-or-string (nth 0 tokens))
                (number-or-string (nth 2 tokens)))))
            ((string= (nth 1 tokens) "OR")
             (setf
               (gethash (nth 4 tokens) gates)
               (list #'logior
                (number-or-string (nth 0 tokens))
                (number-or-string (nth 2 tokens)))))
            ((string= (nth 1 tokens) "LSHIFT")
             (setf
               (gethash (nth 4 tokens) gates)
               (list #'ash
                (number-or-string (nth 0 tokens))
                (number-or-string (nth 2 tokens)))))
            ((string= (nth 1 tokens) "RSHIFT")
             (setf
               (gethash (nth 4 tokens) gates)
               (list (lambda (x y) (logand (ash x (- y)) 65535))
                (number-or-string (nth 0 tokens))
                (number-or-string (nth 2 tokens)))))
            (t 
             (setf
               (gethash (nth 2 tokens) gates)
               (list #'identity (number-or-string (nth 0 tokens))))))))
      (labels
          ((runner (s)
             (let ((v (gethash s registers)))
               (when v (return-from runner v)))
             (let* ((g (gethash s gates))
                    (args (mapcar #'(lambda (x) (if (numberp x) x (runner x))) (rest g)))
                    (res (apply (first g) args)))
               (setf (gethash s registers) res)
               res)))
        (setf silver (runner "a"))
        (setf registers (make-hash-table :test #'equal))
        (setf (gethash "b" registers) silver)
        (setf gold (runner "a"))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))

(defun aoc07-loopy ()
  (let ((silver 0)
        (gold 0))
    (let ((registers (make-hash-table :test #'equal))
          (gates (make-hash-table :test #'equal)))
      (aoc-utils:dolines
        line *standard-input*
        (let ((tokens (str:words line)))
          (cond
            ((string= (nth 0 tokens) "NOT")
             (setf 
              (gethash (list :not (number-or-string (nth 1 tokens))) gates)
              (list nil (nth 3 tokens))))
            ((string= (nth 1 tokens) "AND")
             (setf
               (gethash
                (list
                 :and
                 (number-or-string (nth 0 tokens))
                 (number-or-string (nth 2 tokens)))
                gates)
               (list nil (nth 4 tokens))))
            ((string= (nth 1 tokens) "OR")
             (setf
               (gethash
                (list
                 :or
                 (number-or-string (nth 0 tokens))
                 (number-or-string (nth 2 tokens)))
                gates)
               (list nil (nth 4 tokens))))
            ((string= (nth 1 tokens) "LSHIFT")
             (setf
               (gethash
                (list
                 :lshift
                 (number-or-string (nth 0 tokens))
                 (number-or-string (nth 2 tokens)))
                gates)
               (list nil (nth 4 tokens))))
            ((string= (nth 1 tokens) "RSHIFT")
             (setf
               (gethash
                (list
                 :rshift
                 (number-or-string (nth 0 tokens))
                 (number-or-string (nth 2 tokens)))
                gates)
               (list nil (nth 4 tokens))))

            (t (setf
                 (gethash
                  (list
                   :write
                   (number-or-string (nth 0 tokens)))
                  gates)
                 (list nil (nth 2 tokens)))))))
      (loop
        (let ((dirty 0))
          (maphash
           #'(lambda (k v)
               (unless (first v)
                 (let ((op (first k))
                       (args
                        (mapcar
                         #'(lambda (x)
                             (if (numberp x) x (gethash x registers)))
                         (rest k))))
                   (when (every #'identity args)
                     (incf dirty)
                     (setf (first v) t)
                     (setf (gethash (second v) registers)
                           (case op
                             (:and (logand (first args) (second args)))
                             (:or (logior (first args) (second args)))
                             (:lshift (logand (ash (first args) (second args)) 65535))
                             (:rshift (ash (first args) (- (second args))))
                             (:not (logandc2 65535 (first args)))
                             (:write (first args))))))))
           gates)
          (when (zerop dirty) (return))))
      (setf silver (gethash "a" registers 0))
      ; reset and try with all "b" reads replaced with the value of silver
      (maphash #'(lambda (k v) (setf (first v) nil)) gates)
      (setf registers (make-hash-table :test #'equal))
      (loop
        (let ((dirty 0))
          (maphash
           #'(lambda (k v)
               (unless (first v)
                 (let ((op (first k))
                       (args
                        (mapcar
                         #'(lambda (x)
                             (cond
                              ((numberp x) x)
                              ((string= x "b") silver)
                              (t (gethash x registers))))
                         (rest k))))
                   (when (every #'identity args)
                     (incf dirty)
                     (setf (first v) t)
                     (setf (gethash (second v) registers)
                           (case op
                             (:and (logand (first args) (second args)))
                             (:or (logior (first args) (second args)))
                             (:lshift (logand (ash (first args) (second args)) 65535))
                             (:rshift (ash (first args) (- (second args))))
                             (:not (logandc2 65535 (first args)))
                             (:write (first args))))))))
           gates)
          (when (zerop dirty) (return))))

      (setf gold (gethash "a" registers 0)))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))

(defun aoc07 ()
  (aoc07-memo))
