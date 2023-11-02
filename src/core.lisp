(defpackage :gitwatch
  (:use :cl)
  (:import-from #:cl-workers #:close-and-join-workers)
  (:import-from #:mailer #:start-mailer #:*mailer*)
  (:import-from #:gitwatch/cli #:cli)
  (:import-from #:config/secrets #:db-connect)
  (:import-from #:clingon #:run)
  (:export #:main))
(in-package :gitwatch)

(defun test-mailer ()
  (start-mailer)
  (loop :for i :from 0 :to 5
        :do (mailer:send (write-to-string i)))
  (close-and-join-workers *mailer*)
  )

(defun main (argv)
  (start-mailer)
  (db-connect)
  (run cli argv)
  (print "closing")
  (close-and-join-workers *mailer*))
