(in-package :cl-user)
(defpackage cl-othello
  (:use :cl
        :cl-othello.defines)
  (:export :main)
  (:import-from :cl-othello.utils
                :stream-to-list
                :read-line-while
                :to-string
                :push-without-dup)
  (:import-from :cl-othello.game
                :game-turn
                :init-game
                :is-game-end
                :print-game)
  (:import-from :cl-othello.player
                :player-name
                :player-kind
                :player-params
                :player-serialize
                :player-deserialize
                :player-make-mover
                :construct-player
                :find-player-by-name
                :player-set-param)
  (:import-from :alexandria
                :make-keyword))
  
(in-package :cl-othello)

(defparameter *player-file* "PLAYER_INFO")

(defun main (&key (plyr-file *player-file*) (in *standard-input*))
  (let ((game (init-game))
        (plyr-list (com-load-player plyr-file))
        (input-list nil)
        (com-name nil)
        (arg-list nil))
    (loop until (eq (make-keyword com-name) :quit) do
         (princ "> " )
         (setf input-list (stream-to-list in))
         (setf com-name (car input-list))
         (setf arg-list (cdr input-list))
         (case (make-keyword com-name)
           ((:help :nil) (com-help))
           (:show (print-game game))
           (:init (setf game (init-game))
                  (print-game game))
           (:start (com-start-game game arg-list plyr-list))
           (:player (setf plyr-list (com-player arg-list plyr-list in))
                    (com-save-player plyr-file plyr-list))
           (:quit)
           (t (format t "The command \"~D\" is not defined~%" com-name))))))

(defun com-help ()
  (labels ((princ-line (str)
             (princ str) (fresh-line)))
    (princ-line "init ; initialize game")
    (princ-line "show ; show game-board")
    (princ-line "start [player1] [player2] ; start game")
    (princ-line "quit")
    (princ-line "player")
    (com-player-help "  ")))

(defun com-start-game (game com-list plyr-list)
  (let ((mover1 (make-mover-by-name-sym plyr-list (car com-list)))
        (mover2 (make-mover-by-name-sym plyr-list (cadr com-list))))
    (when (or (null mover1) (null mover2))
      (format t "2 existing player names are required")
      (return-from com-start-game (values game nil)))
    (values (game-loop game mover1 mover2) t)))

(defun make-mover-by-name-sym (plyr-list name-sym)
  (when name-sym 
    (let ((plyr (find-if (lambda (target) (equalp (player-name target)
                                                  (symbol-name name-sym)))
                         plyr-list)))
      (when plyr
        (player-make-mover plyr)))))

(defun game-loop (game white-mover black-mover)
  (loop until (is-game-end game) do
       (if (= (game-turn game) +white+)
           (funcall white-mover game)
           (funcall black-mover game)))
  game)

;-----commands about player -----;

;; TODO: change the interface of adding player (to receive a player's name)
(defun com-player (com-list plyr-list &optional (stream *standard-input*))
  (let ((valid-com t)
        (com-keyword (make-keyword (car com-list))))
    (case com-keyword
      ((:nil :help) (com-player-help))
      (:show (com-show-player plyr-list))
      (:remove (multiple-value-bind (lst suc) (com-remove-player (symbol-name (cadr com-list)) plyr-list)
                 (if suc
                     (setf plyr-list lst)
                     (format t "The name \"~D\" is not exist~%" (cadr com-list)))))
      (:add (setf plyr-list
                  (com-add-player (com-init-player) plyr-list stream)))
      (t (format t "The command \"~D\" is not defined" com-keyword)
         (setf valid-com nil)))
    (values plyr-list valid-com)))

(defun com-player-help (&optional (prefix ""))
  (labels ((princ-line (str)
             (princ (concatenate 'string prefix "player " str))
             (fresh-line)))
    (princ-line "show")
    (princ-line "remove [name]")
    (princ-line "add")))

(defun com-load-player (file-name)
  (when (probe-file file-name) 
    (with-open-file (in file-name :direction :input)
      (let (str result)
        (loop while (setf str (read-line in nil)) do
             (setf result (cons (player-deserialize str) result)))
        result))))

(defun com-save-player (file-name player-list)
  (with-open-file (out file-name
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (dolist (plyr player-list)
      (princ (player-serialize plyr) out)
      (fresh-line out))))

(defun com-show-player (player-list &optional (stream *standard-output*))
  (dolist (plyr player-list)
    (format stream "~A (~A) -> " (player-name plyr) (player-kind plyr))
    (maphash (lambda (k v)
               (format stream "~A = ~A, " k (to-string v)))
             (player-params plyr))
    (fresh-line stream)))

(defun com-remove-player (name player-list)
  (when (equalp name "")
    (princ "Please input the name to delete")
    (return-from com-remove-player (values player-list nil)))
  (let ((found (find-player-by-name name player-list)))
    (when (null found)
      (format t "The name '~D' is not found~%" name)
      (return-from com-remove-player (values player-list nil)))
    (values (remove found player-list) t)))

(defun com-add-player (plyr plyr-list &optional (stream *standard-input*))
  (labels ((avoids-overwrite ()
             (and (find-player-by-name (player-name plyr) plyr-list)
                  (equalp
                   (read-line-while "The name is already exist. Do you overwrite it? [y/n]"
                                    (lambda (str) (not (or (equalp str "y")
                                                           (equalp str "n"))))
                                    stream)
                   "n"))))
    (unless (or (null plyr)
                (avoids-overwrite)) 
      (com-modify-player plyr stream)
      (setf plyr-list (push-without-dup plyr plyr-list
                                        (lambda (a b) (equalp (player-name a) (player-name b)))))))
  plyr-list)

(defun com-init-player (&optional (stream *standard-input*))
  (let* ((name (read-line-while "name"
                                (lambda (str)
                                  (equalp str ""))
                                stream))
         (plyr nil))
    (read-line-while "kind [human, random, mc, uct]"
                     (lambda (str)
                       (setf plyr (construct-player name (intern (string-upcase str))))
                       (null plyr))
                     stream)
    plyr))

(defun com-modify-player (plyr &optional (stream *standard-input*))
  (maphash (lambda (k v)
             (read-line-while
              (format nil "~D (default=~D)" k (to-string v))
              (lambda (str)
                (not (or (equalp str "")
                         (handler-case
                             (player-set-param plyr k
                                               (read-from-string str))
                           (simple-error (e) (declare (ignore e)) nil)))))
              stream))
           (player-params plyr))
  plyr)

