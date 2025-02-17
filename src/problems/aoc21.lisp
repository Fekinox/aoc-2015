(in-package :aoc-problems)

(defun aoc21 ()
  (let ((silver nil)
        (gold 0))
    (defstruct player-state
      gold damage armor has-weapon has-armor rings)
    (let* ((groups (aoc-utils:newline-separated-groups *standard-input*))
           (weapons (make-array (1- (length (first groups))) :fill-pointer 0))
           (armor (make-array (1- (length (second groups))) :fill-pointer 0))
           (rings (make-array (1- (length (third groups))) :fill-pointer 0))

           (silver-queue (dfpq:make-queue))
           (gold-queue (dfpq:make-queue))

           (init-health 100)
           (init-damage 0)
           (init-armor 0)

           (boss-health 0)
           (boss-damage 0)
           (boss-armor 0))
      (dolist (l (rest (first groups)))
        (vector-push (mapcar #'parse-integer (rest (str:words l))) weapons))
      (dolist (l (rest (second groups)))
        (vector-push (mapcar #'parse-integer (rest (str:words l))) armor))
      (dolist (l (rest (third groups)))
        (vector-push (mapcar #'parse-integer (rest (rest (str:words l)))) rings))
      (let ((boss-stats (fourth groups)))
        (setf boss-health (parse-integer (third (str:words (first boss-stats))))
              boss-damage (parse-integer (second (str:words (second boss-stats))))
              boss-armor (parse-integer (second (str:words (third boss-stats))))))

      (dotimes (i (length weapons))
        (destructuring-bind (w-cost w-damage w-armor) (aref weapons i)
          (dfpq:enqueue silver-queue
                    (make-player-state
                      :gold w-cost
                      :damage w-damage
                      :armor w-armor
                      :has-weapon t)
                    w-cost)))
      (loop
        (multiple-value-bind (pl found-p) (dfpq:dequeue silver-queue)
          (unless found-p (return))
          (if
              (block can-beat-boss
                (let ((boss-hp boss-health)
                      (your-hp init-health)
                      (your-damage (player-state-damage pl))
                      (your-armor (player-state-armor pl)))
                  (loop
                    (decf boss-hp (max 1 (- your-damage boss-armor)))
                    (when (<= boss-hp 0) (return-from can-beat-boss t))
                    (decf your-hp (max 1 (- boss-damage your-armor)))
                    (when (<= your-hp 0) (return-from can-beat-boss nil)))))
            (when (null silver) (setf silver (player-state-gold pl)))
            (when (> (player-state-gold pl) gold)
              (format t "~a~%" pl)
              (setf gold (player-state-gold pl))))
          (unless (player-state-has-weapon pl)
            (dotimes (i (length weapons))
              (destructuring-bind (cost damage armor) (aref weapons i)
                (dfpq:enqueue silver-queue
                              (make-player-state
                                :gold (+ (player-state-gold pl) cost)
                                :damage (+ (player-state-damage pl) damage)
                                :armor (+ (player-state-armor pl) armor)
                                :has-weapon t
                                :has-armor (player-state-has-armor pl)
                                :rings (player-state-rings pl))
                              (+ (player-state-gold pl) cost)))))
          (unless (player-state-has-armor pl)
            (dotimes (i (length armor))
              (destructuring-bind (cost damage armor) (aref armor i)
                (dfpq:enqueue silver-queue
                              (make-player-state
                                :gold (+ (player-state-gold pl) cost)
                                :damage (+ (player-state-damage pl) damage)
                                :armor (+ (player-state-armor pl) armor)
                                :has-weapon (player-state-has-weapon pl)
                                :has-armor t
                                :rings (player-state-rings pl))
                              (+ (player-state-gold pl) cost)))))
          (unless (>= (length (player-state-rings pl)) 2)
            (dotimes (i (length rings))
              (unless (find i (player-state-rings pl))
                (destructuring-bind (cost damage armor) (aref rings i)
                  (dfpq:enqueue silver-queue
                                (make-player-state
                                  :gold (+ (player-state-gold pl) cost)
                                  :damage (+ (player-state-damage pl) damage)
                                  :armor (+ (player-state-armor pl) armor)
                                  :has-weapon (player-state-has-weapon pl)
                                  :has-armor (player-state-has-armor pl)
                                  :rings (cons i (player-state-rings pl)))
                                (+ (player-state-gold pl) cost))))))))

      (dotimes (wi (length weapons))
        (dotimes (ai (1+ (length armor)))
          (destructuring-bind
              ((w-cost w-damage w-armor)
               (a-cost a-damage a-armor))
              (list 
                (aref weapons wi)
                (if (< wi (length armor)) (aref armor wi) '(0 0 0)))
            (dfpq:enqueue gold-queue
                          (make-player-state
                            :gold (+ w-cost a-cost)
                            :damage (+ w-damage a-damage)
                            :armor (+ w-armor a-armor))
                          (- 999999 (+ w-cost a-cost)))
            (dotimes (ri1 (length rings))
              (destructuring-bind
                  (r1-cost r1-damage r1-armor) (aref rings ri1)
                (dfpq:enqueue gold-queue
                              (make-player-state
                                :gold (+ w-cost a-cost r1-cost)
                                :damage (+ w-damage a-damage r1-damage)
                                :armor (+ w-armor a-armor r1-armor))
                              (- 99999 (+ w-cost a-cost r1-cost)))
                (dotimes (ri2 ri1)
                  (destructuring-bind
                      (r2-cost r2-damage r2-armor) (aref rings ri2)
                    (dfpq:enqueue gold-queue
                                  (make-player-state
                                    :gold (+ w-cost a-cost r1-cost r2-cost)
                                    :damage (+ w-damage a-damage r1-damage r2-damage)
                                    :armor (+ w-armor a-armor r1-armor r2-armor))
                                  (- 99999 (+ w-cost a-cost r1-cost r2-cost))))))))))

      ; (loop
      ;   (multiple-value-bind (pl found-p) (dfpq:dequeue gold-queue)
      ;     (unless found-p (return))
      ;     (unless
      ;         (block can-beat-boss
      ;           (let ((boss-hp boss-health)
      ;                 (your-hp init-health)
      ;                 (your-damage (player-state-damage pl))
      ;                 (your-armor (player-state-armor pl)))
      ;             (loop
      ;               (decf boss-hp (max 1 (- your-damage boss-armor)))
      ;               (when (<= boss-hp 0) (return-from can-beat-boss t))
      ;               (decf your-hp (max 1 (- boss-damage your-armor)))
      ;               (when (<= your-hp 0) (return-from can-beat-boss nil)))))
      ;       (setf gold (player-state-gold pl))
      ;       (return))))
      )
    (format *standard-output* "silver: ~a~%" silver)
    (format *standard-output* "gold: ~a~%" gold)))
