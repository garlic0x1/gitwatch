(defpackage :gitwatch
  (:use :cl)
  (:export #:main))
(in-package :gitwatch)

(defun main (&rest argv)
  (mailer:start-mailer)
  (mailer:alert-on-fail "gitwatch:main"
    (config/secrets:db-connect)
    (clingon:run gitwatch/cli:cli argv))

  ;; wait for end
  (cl-workers:join-workers mailer::*mailer*))
