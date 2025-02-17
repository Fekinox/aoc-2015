(in-package :aoc-utils)

(defmacro with-default-input (path &body body)
  (let ((path-sym (gensym)))
    `(let ((,path-sym ,path))
       (if (string= ,path-sym "-")
           (progn ,@body)
           (uiop:with-input-file (*standard-input* ,path-sym) ,@body)))))

(defmacro with-default-output (path &body body)
  (let ((path-sym (gensym)))
    `(let ((,path-sym ,path))
       (if (string= ,path-sym "-")
           (progn ,@body)
           (uiop:with-output-file (*standard-output* ,path-sym) ,@body)))))

; -: standard input
; filename: filename
(defun run-problem (p &optional (input "-") (output "-"))
  (with-default-input input
      (with-default-output output
        (funcall p)))) 
