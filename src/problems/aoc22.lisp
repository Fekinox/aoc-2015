(in-package :aoc-problems)

(defun aoc22 ()
  (let ((silver 0)
        (gold 0))
    (let* ((boss-health (parse-integer (third (str:words (read-line)))))
           (boss-damage (parse-integer (second (str:words (read-line)))))
           (silver-queue (dfpq:make-queue))
           (gold-queue (dfpq:make-queue)))
      (defstruct wizard-state
        health
        mana
        your-turn
        spent-mana
        boss-health
        effects)
      (dfpq:enqueue silver-queue
                    (make-wizard-state
                      :health 50
                      :mana 500
                      :spent-mana 0
                      :boss-health boss-health
                      :your-turn t)
                    0)

      (dfpq:enqueue gold-queue 
                    (make-wizard-state
                      :health 50
                      :mana 500
                      :spent-mana 0
                      :boss-health boss-health
                      :your-turn t)
                    0)
      (labels
          ((apply-effects (ws)
             (let ((new-state (copy-wizard-state ws)))
               (setf (wizard-state-effects new-state) nil)
               (dolist (ef (wizard-state-effects ws))
                 (case (car ef)
                   (:poison
                    (decf (wizard-state-boss-health new-state) 3))
                   (:recharge
                    (incf (wizard-state-mana new-state) 101)))
                 (when (> (cdr ef) 1)
                   (push (cons (car ef) (1- (cdr ef)))
                         (wizard-state-effects new-state))))
               new-state))
           (cast-spell (ws spell)
             (let ((new-state (copy-wizard-state ws)))
               (setf (wizard-state-your-turn new-state) nil)
               (cond 
                 ((and
                   (eq spell :missile)
                   (>= (wizard-state-mana ws) 53))
                  (decf (wizard-state-mana new-state) 53)
                  (incf (wizard-state-spent-mana new-state) 53)
                  (decf (wizard-state-boss-health new-state) 4))
                 ((and
                    (eq spell :drain)
                    (>= (wizard-state-mana ws) 73))
                  (decf (wizard-state-mana new-state) 73)
                  (incf (wizard-state-spent-mana new-state) 73)
                  (decf (wizard-state-boss-health new-state) 2)
                  (incf (wizard-state-health new-state) 2))
                 ((and
                    (eq spell :shield)
                    (>= (wizard-state-mana ws) 113)
                    (not (assoc :shield (wizard-state-effects ws))))
                  (decf (wizard-state-mana new-state) 113)
                  (incf (wizard-state-spent-mana new-state) 113)
                  (push (cons :shield 6) (wizard-state-effects new-state)))
                 ((and
                    (eq spell :poison)
                    (>= (wizard-state-mana ws) 173)
                    (not (assoc :poison (wizard-state-effects ws))))
                  (decf (wizard-state-mana new-state) 173)
                  (incf (wizard-state-spent-mana new-state) 173)
                  (push (cons :poison 6) (wizard-state-effects new-state)))
                 ((and
                    (eq spell :recharge)
                    (>= (wizard-state-mana ws) 229)
                    (not (assoc :recharge (wizard-state-effects ws))))
                  (decf (wizard-state-mana new-state) 229)
                  (incf (wizard-state-spent-mana new-state) 229)
                  (push (cons :recharge 5) (wizard-state-effects new-state)))
                 (t (setf new-state nil)))
               new-state))
           (boss-turn (ws)
             (let ((new-state (copy-wizard-state ws))
                   (armor (if (assoc :shield (wizard-state-effects ws))
                              7
                              0)))
               (setf (wizard-state-your-turn new-state) t)
               (decf (wizard-state-health new-state) (max 1 (- boss-damage armor)))
               new-state))
           (boss-dead (ws)
             (<= (wizard-state-boss-health ws) 0))
           (player-dead (ws)
             (<= (wizard-state-health ws) 0)))
        (loop
          (tagbody
            top
            (multiple-value-bind (pl found-p) (dfpq:dequeue silver-queue)
              (unless found-p (return))
              (format t "~a~%" (wizard-state-spent-mana pl))
              (when (boss-dead pl)
                (setf silver (wizard-state-spent-mana pl))
                (return))
              (when (player-dead pl)
                (go top))
              (let ((effects-ws (apply-effects pl)))
                (when (boss-dead effects-ws)
                  (setf silver (wizard-state-spent-mana effects-ws))
                  (return))
                (if (wizard-state-your-turn effects-ws) 
                    (let ((your-turn-states
                           (remove-if
                             #'null
                             (mapcar
                              #'(lambda (s)
                                  (cast-spell effects-ws s))
                              '(:missile :drain :shield :poison :recharge)))))
                      (dolist (st your-turn-states)
                        (dfpq:enqueue silver-queue
                                      st
                                      (wizard-state-spent-mana st))))
                  (let ((boss-ws (boss-turn effects-ws)))
                    (when (player-dead boss-ws)
                      (go top))
                    (dfpq:enqueue silver-queue
                                  boss-ws 
                                  (wizard-state-spent-mana boss-ws))))))))
        (loop
          (tagbody
            top
            (multiple-value-bind (pl found-p) (dfpq:dequeue gold-queue)
              (unless found-p (return))
              (format t "~a~%" (wizard-state-spent-mana pl))
              (when (wizard-state-your-turn pl)
                (decf (wizard-state-health pl)))
              (when (boss-dead pl)
                (setf gold (wizard-state-spent-mana pl))
                (return))
              (when (player-dead pl)
                (go top))
              (let ((effects-ws (apply-effects pl)))
                (when (boss-dead effects-ws)
                  (setf gold (wizard-state-spent-mana effects-ws))
                  (return))
                (if (wizard-state-your-turn effects-ws) 
                    (let ((your-turn-states
                           (remove-if
                             #'null
                             (mapcar
                              #'(lambda (s)
                                  (cast-spell effects-ws s))
                              '(:missile :drain :shield :poison :recharge)))))
                      (dolist (st your-turn-states)
                        (dfpq:enqueue gold-queue
                                      st
                                      (wizard-state-spent-mana st))))
                  (let ((boss-ws (boss-turn effects-ws)))
                    (when (player-dead boss-ws)
                      (go top))
                    (dfpq:enqueue gold-queue
                                  boss-ws 
                                  (wizard-state-spent-mana boss-ws)))))))))
      (format t "~a ~a~%" boss-health boss-damage))
    (format *standard-output* "silver: ~a~%" silver)
    (format *standard-output* "gold: ~a~%" gold)))
