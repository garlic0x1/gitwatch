(defpackage :gitwatch
  (:use :cl)
  (:import-from #:cl-workers #:close-and-join-workers)
  (:import-from #:mailer #:start-mailer #:*mailer*)
  (:export #:main))
(in-package :gitwatch)

(defun main (argv)
  (start-mailer)
  ;; (mailer:alert-on-fail "gitwatch:main")
  (config/secrets:db-connect)
  (clingon:run gitwatch/cli:cli argv)

  ;; wait for end
  (close-and-join-workers mailer::*mailer*))

(defun test-mailer ()
  (start-mailer)
  (loop :for i :from 0 :to 5
        :do (mailer:send (write-to-string i)))
  (close-and-join-workers mailer::*mailer*)
  )
