(defpackage mailer
  (:use :cl :alexandria-2)
  (:export #:send #:alert-on-fail))
(in-package :mailer)

;;
;; Client helper functions
;;

(defun discord-send (message &key (username "*gitwatch*") (hook config/secrets:discord))
  (dex:post hook :content `(("username" . ,username) ("content" . ,message))))

;;
;; Log errors to Discord
;;

(defmacro alert-on-fail (name &body forms)
  `(handler-case (progn ,@forms)
     (error (c)
       (discord-send (format nil "procedure: `~s`~%error: ~s" ,name c)
                     :hook config/secrets:discord-errors)
       nil)))

;;
;; Send a data model to discord
;;

(defgeneric send (obj)
  (:method ((obj null)) nil)

  (:method ((obj string)) (discord-send obj))

  (:method ((obj db:last-commit))
    (discord-send
     (format nil "repo: **~a**~%commit: ~a~%time: ~a~%link: ~a" (utils:pretty-repo (db::last-commit-repo obj)) (db::last-commit-message obj) (db::last-commit-time obj) (db::last-commit-link obj))
     :username (db::last-commit-author obj))))
