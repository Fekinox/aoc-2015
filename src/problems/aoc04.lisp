(in-package :aoc-problems)

(defun aoc04 ()
  (let ((silver nil)
        (gold nil))
    (let* ((l (read-line))
           (n 1)
           (create-test-line 
            (lambda (n)
              (map 
               '(vector (unsigned-byte 8) *) #'char-code
               (concatenate 'string l (write-to-string n)))))
           (get-md5 (lambda (n) (ironclad:digest-sequence :md5 (funcall create-test-line n))))
           (silver-good 
            (lambda (s) 
              (and (zerop (aref s 0)) (zerop (aref s 1)) (< (aref s 2) 16))))
           (gold-good
            (lambda (s) 
              (and (zerop (aref s 0)) (zerop (aref s 1)) (zerop (aref s 2))))))
      (loop
        (let ((hash (funcall get-md5 n)))
          (when (and (null silver) (funcall silver-good hash))
            (setf silver n))
          (when (and (null gold) (funcall gold-good hash))
            (setf gold n)))
        (when (and silver gold)
          (return))
        (incf n)))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
