(defpackage gitwatch/actors
  (:use :cl :cl-workers)
  (:export #:start-actors #:*mailer*))
(in-package :gitwatch/actors)

;;
;; Client actor
;; Messages are rate limited to no more than one per second
;;

(defvar *mailer* nil)
(defworker mailer () (hook payload)
  (sleep 1)
  (dex:post hook :content payload))

(defun start-actors ()
  (setf *mailer* (mailer-actor))
  ;; (setf *scraper* (scraper-actor))
  )
