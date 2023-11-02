(defpackage :gitwatch
  (:use :cl)
  (:import-from #:gitwatch/cli #:cli)
  (:import-from #:config/secrets #:db-connect)
  (:import-from #:clingon #:run)
  (:export #:main))
(in-package :gitwatch)

(defun main (argv)
  (mailer:alert-on-fail :cli-entrypoint
    (db-connect)
    (run cli argv)))
