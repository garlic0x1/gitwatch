(defpackage #:config/secrets
  (:use :cl)
  (:export #:db-connect #:discord-errors #:discord))
(in-package :config/secrets)

;;
;; Establish database connection
;;

(defun db-connect ()
  (mito:connect-toplevel
   :postgres
   :username "postgres"
   :password "postgres"
   :host "localhost"
   :database-name "debug_test"))

;;
;; Set discord hook for notifications
;;

(defvar discord
  "https://discord.com/api/webhooks/1167371572424753274/Ox8iitQ53rRmpNNZA4QAtFOYA68KWsNi9t3N2nB7p5JLGP1oiTiUpX-It38MCoU-nE6a")

(defvar discord-errors
  "https://discord.com/api/webhooks/1167371572424753274/Ox8iitQ53rRmpNNZA4QAtFOYA68KWsNi9t3N2nB7p5JLGP1oiTiUpX-It38MCoU-nE6a")
