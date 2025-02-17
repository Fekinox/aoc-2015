(in-package :aoc-utils)

(defmacro dolines (var stream &body body)
  (let ((stream-sym (gensym)))
    `(let ((,stream-sym ,stream))
       (loop
         (multiple-value-bind (,var eof) (read-line ,stream-sym nil)
           (when eof (return))
           ,@body)))))

(defun newline-separated-groups (stream)
  (let ((groups nil)
        (cur-group nil))
    (dolines line stream
        (if (string= line "")
          (unless (null cur-group)
            (push (nreverse cur-group) groups)
            (setf cur-group nil))
          (push line cur-group)))
    (unless (null cur-group)
      (push (nreverse cur-group) groups))
    (nreverse groups)))
