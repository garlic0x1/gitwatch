(defpackage mailer
  (:use :cl :alexandria-2)
  (:import-from #:cl-workers #:spawn-worker #:close-and-join-workers)
  (:export #:send #:alert-on-fail #:with-mailer))
(in-package :mailer)

;;
;; Client actor convenience macro
;;

(defmacro with-mailer (&body body)
  `(progn
     (spawn-worker :mailer () (hook payload)
       (dex:post hook :content payload)
       (sleep 1))
     ,@body
     (close-and-join-workers :mailer)))

;;
;; Send signal to mailer actor
;;

(defun discord-send (message &key (username "*gitwatch*") (hook config/secrets:discord))
  (cl-workers:send :mailer hook `(("username" . ,username)
                                  ("content" . ,message))))

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
    (with-slots (repo message time link author) obj
      (discord-send
       (format nil "repo: **~a**~%commit: ~a~%time: ~a~%link: ~a" (utils:pretty-repo repo) message time link)
       :username author))))
