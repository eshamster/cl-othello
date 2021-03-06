(in-package :cl-user)
(defpackage cl-othello.uct
  (:use :cl
        :cl-othello.defines
        :cl-othello.tree)
  (:export :uct-simulate
           :uct-node-num
           :uct-node-sum
           :expand-child-if-needed
           :reflect-sim-result
           :select-uct-child
           :make-uct-param
           :uct-param-expand-intv
           :uct-param-ucb-coef
           :make-default-uct-param
           :*default-uct-param*
           :mcts-simulate-once)
  (:import-from :cl-othello.move
                :clone-move)
  (:import-from :cl-othello.move-store
                :move-store-count
                :mapcar-move-store)
  (:import-from :cl-othello.game
                :game-board
                :game-turn
                :make-moves
                :do-in-move-reverse)
  (:import-from :cl-othello.random-move
                :make-uniform-policy)
  (:import-from :cl-othello.mc
                :calc-ucb
                :mc-simulate-once))
(in-package :cl-othello.uct)

(defstruct uct-node
  unexpanded-moves
  move-from-parent
  num
  sum)

(defun make-a-uct-node (move-from-parent)
  (list (make-uct-node
         :unexpanded-moves :has-not-made-child
         :move-from-parent move-from-parent
         :num 0
         :sum 0)))

; all evaluated values are from white player's perspective

(defstruct uct-param
  expand-intv
  ucb-coef
  fn-make-policy)

(defun make-default-uct-param ()
  (make-uct-param :expand-intv 2
                  :ucb-coef 1
                  :fn-make-policy 'make-uniform-policy))
  
(defparameter *default-uct-param* (make-default-uct-param))

; This function is destructive
(defun expand-child-if-needed (game tree uct-param)
  (let* ((node (get-node-value tree))
         (moves (uct-node-unexpanded-moves node))
         (expand-intv (uct-param-expand-intv uct-param)))
    (when (eq moves :has-not-made-child)
      (setf moves (mapcar-move-store (lambda (m) (clone-move m))
                                     (make-moves game)))
      (setf (uct-node-unexpanded-moves node) moves))
    (let ((move (car moves)))
      (when (and move
                 (> (uct-node-num node) 0)
                 (eq (mod (uct-node-num node) expand-intv) (- expand-intv 1))) 
        (setf (uct-node-unexpanded-moves node) (cdr moves))
        (push-child tree (make-a-uct-node move)))))
  tree)
    
(defun select-uct-child (game parent uct-param)
  (when (has-children parent) 
    (let ((ucb-coef (uct-param-ucb-coef uct-param))
          (now-turn (game-turn game)))
      (select-max-child (lambda (node)
                          (calc-ucb (uct-node-sum node)
                                    (uct-node-num node)
                                    (uct-node-num (get-node-value parent))
                                    :coef  ucb-coef
                                    :turn now-turn))
                        parent))))

(defun reflect-sim-result (game uct-tree uct-param result)
  (declare (ignore game uct-param))
  (incf (uct-node-sum (get-node-value uct-tree)) result)
  (incf (uct-node-num (get-node-value uct-tree)))
  (values result uct-tree))

(defun mcts-simulate-once (game tree param &key
                                             (fn-select #'select-uct-child)
                                             (fn-expand #'expand-child-if-needed)
                                             (fn-simulate #'mc-simulate-once)
                                             (fn-backprop #'reflect-sim-result))
  (labels ((sim-once (node)
             (funcall fn-expand game node param)
             (let ((child-tree (funcall fn-select game node param)))
               (funcall fn-backprop
                        game
                        node
                        param
                        (if (null child-tree)
                            (funcall fn-simulate game (uct-param-fn-make-policy param))
                            (do-in-move-reverse
                                game
                                (uct-node-move-from-parent (get-node-value child-tree))
                              (sim-once child-tree)))))))
    (multiple-value-bind (result tree) (sim-once tree)
      (declare (ignore result))
      tree)))

(defun select-uct-node-by-ave (turn uct-tree)
  (select-max-child (lambda (node)
                      (let ((num (uct-node-num node))
                            (sum (uct-node-sum node)))
                        (if (> num 0)
                            (* (/ sum num) (if (= turn +white+) 1 -1))
                            -99999)))
                    uct-tree))
                    
(defun uct-simulate (game param times)
  (let ((uct-tree (make-a-uct-node nil)))
    (dotimes (i times)
      (mcts-simulate-once game uct-tree param))
    (uct-node-move-from-parent
     (get-node-value (select-uct-node-by-ave (game-turn game) uct-tree)))))

