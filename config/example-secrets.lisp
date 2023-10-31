(defpackage #:config/secrets
  (:use :cl)
  (:export #:db-connect #:discord-errors #:discord))
(in-package :config/secrets)

;;
;;
;;  MODIFY THIS FILE AND MOVE TO ./config/secrets.lisp
;;
;;

;;
;; Establish database connection
;;

(defun db-connect ()
  (mito:connect-toplevel
   :postgres
   :username "postgres"
   :password "postgres"
   :host "localhost"
   :database-name "dbname"))

;;
;; Set discord hook for notifications
;;

(defvar discord "https://discord.com/api/webhooks/blah/blah")

(defvar discord-errors "https://discord.com/api/webhooks/blah/blah")
