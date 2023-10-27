(defpackage mailer
  (:use :cl :alexandria-2)
  (:export #:send))
(in-package :mailer)

;;
;; Client helper functions
;;

(defun discord-send (message &optional username)
  (dex:post config/secrets:discord
            :content `(("username" . ,(or username "*gitwatch*"))
                       ("content" . ,message))))

;;
;; Send a data model to discord
;;

(defgeneric send (obj)
  (:method ((obj null)) nil)

  (:method ((obj string)) (discord-send obj))

  (:method ((obj db:commit))
    (discord-send
     (format nil "~a" (db::commit-link obj))
     (db::commit-author obj)))

  (:method ((obj db:last-commit))
    (discord-send
     (format nil "commit: ~a~%time: ~a~%link: ~a" (db::last-commit-message obj) (db::last-commit-time obj) (db::last-commit-link obj))
     (db::last-commit-author obj)))

  (:method ((obj db:issue))
    (discord-send
     (format nil "~a" (db::issue-link obj))
     (db::issue-author obj)))

  (:method ((obj db:pull-request))
    (discord-send
     (format nil "~a" (db::pull-request-link obj))
     (db::pull-request-author obj))))
