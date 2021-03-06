(in-package :cl-user)
(defpackage cl-othello.mc
  (:use :cl
        :cl-othello.defines)
  (:export :calc-ucb
           :mc-simulate-once
           :mc-simulate)
  (:import-from :cl-othello.tree
                :select-max-node)
  (:import-from :cl-othello.move
                :clone-move)
  (:import-from :cl-othello.move-store
                :mapcar-move-store)
  (:import-from :cl-othello.game
                :make-moves
                :game-turn
                :is-game-end
                :get-game-depth
                :get-game-result
                :reverse-game-to-depth
                :do-in-move-reverse)
  (:import-from :cl-othello.random-move
                :make-prob-store
                :move-by-random-policy
                :make-uniform-policy))
(in-package :cl-othello.mc)

(defconstant +ucb-max+ 99999)
(defconstant +ucb-min+ -99999)

(defun sim-to-game-end (game fn-make-policy prob-store)
  (if (is-game-end game)
      game
      (sim-to-game-end (move-by-random-policy game fn-make-policy :prob-store prob-store)
                       fn-make-policy prob-store)))
  
(defun mc-simulate-once (game fn-make-policy &key (prob-store (make-prob-store)))
  (let ((depth (get-game-depth game))
        (result (get-game-result (sim-to-game-end game fn-make-policy prob-store))))
    (reverse-game-to-depth game depth)
    result))

; use UCB
(defstruct mc-node
  move
  sum
  num)

(defun calc-ucb (sum num total-num &key (coef 1.41421356) (turn +white+))
  (if (and (< 0 num) (< 0 total-num))
      (+ (* (/ sum num)
            (if (= turn +white+) 1 -1))
         (* coef (sqrt (/ (log total-num) num))))
      +ucb-max+))

(defun init-mc-nodes (game)
  (mapcar-move-store (lambda (move)
                       (make-mc-node :move (clone-move move)
                                     :sum 0
                                     :num 0))
                     (make-moves game)))

(defun select-mc-node-by-ucb (mc-nodes total-num)
  (select-max-node (lambda (node)
                     (calc-ucb (mc-node-sum node) (mc-node-num node) total-num))
                   mc-nodes))

(defun select-mc-node-by-ave (mc-nodes)
  (select-max-node (lambda (node)
                     (let ((num (mc-node-num node))
                           (sum (mc-node-sum node)))
                       (if (< 0 num)
                           (/ sum num)
                           +ucb-min+)))
                   mc-nodes))

(defun mc-simulate (game fn-make-policy times)
  (let ((mc-nodes (init-mc-nodes game))
        (prob-store (make-prob-store))
        (turn (game-turn game)))
    (when mc-nodes
      (dotimes (now-times times)
        (let* ((node (select-mc-node-by-ucb mc-nodes now-times))
               (move (mc-node-move node)))
          (do-in-move-reverse game move
            (let ((result (mc-simulate-once game fn-make-policy :prob-store prob-store)))
              (cond ((= result turn) (incf (mc-node-sum node)))
                    ((= result (reverse-turn turn)) (decf (mc-node-sum node)))))
            (incf (mc-node-num node)))))
      (mc-node-move (select-mc-node-by-ave mc-nodes)))))
