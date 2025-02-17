(in-package :aoc-problems)

(defun aoc14 ()
  (let ((silver 0)
        (gold 0))
    (defstruct state
      time is-active)
    (defstruct reindeer
      speed duration rest pos points state)
    (let ((reindeer nil))
      (aoc-utils:dolines
        line *standard-input*
        (let* ((tokens (str:words line))
               (speed (parse-integer (nth 3 tokens)))
               (duration (parse-integer (nth 6 tokens)))
               (rest-duration (parse-integer (nth 13 tokens))))
          (push (make-reindeer
                  :speed speed
                  :duration duration
                  :rest rest-duration
                  :pos 0
                  :points 0
                  :state (make-state :time duration :is-active t)) reindeer)))
      (format t "~a~%" reindeer)
      (dotimes (i 2503)
        (dolist (r reindeer)
          (when (state-is-active (reindeer-state r))
            (incf (reindeer-pos r) (reindeer-speed r)))
          (decf (state-time (reindeer-state r)))
          (when (zerop (state-time (reindeer-state r)))
            (if (state-is-active (reindeer-state r))
                (setf (state-time (reindeer-state r))
                      (reindeer-rest r)
                      (state-is-active (reindeer-state r))
                      nil)
                (setf (state-time (reindeer-state r))
                      (reindeer-duration r)
                      (state-is-active (reindeer-state r))
                      t))))
        (let ((best-pos 0))
          (dolist (r reindeer)
            (setf best-pos (max best-pos (reindeer-pos r))))
          (dolist (r reindeer)
            (when (= (reindeer-pos r) best-pos)
              (incf (reindeer-points r))))))
      (dolist (r reindeer)
        (setf
          silver (max silver (reindeer-pos r))
          gold (max gold (reindeer-points r)))))
    (format *standard-output* "silver: ~A~%" silver)
    (format *standard-output* "gold: ~A~%" gold)))
