(defpackage db
  (:use :cl)
  (:export #:migrate #:repository #:last-commit #:commit #:issue #:pull-request))
(in-package :db)

;;
;; Establish database connection
;; SQLite database is touched in Makefile so truename works
;;

(defparameter *db*
  (mito:connect-toplevel
   :postgres
   :username "postgres"
   :password "postgres"
   :host "localhost"
   :database-name "debug_test"
   ;; (truename "~/.cache/git-notify.sqlite3")
   ))

;;
;; Migration helpers
;;

(defvar *db-tables* nil)

(defmacro deftable (name direct-superclasses direct-slots &rest options)
  `(progn
     (setf *db-tables* (cons (quote ,name) *db-tables*))
     (mito:deftable ,name ,direct-superclasses ,direct-slots ,@options)))

(defun migrate ()
  (loop :for table :in *db-tables*
        :do (mito:recreate-table table)))

;;
;; Data models
;;

(deftable repository ()
  ((link :col-type (:varchar 256) :primary-key t)
   (host :col-type (:varchar 32))
   (user :col-type (:varchar 128))
   (repo :col-type (:varchar 128))))

(deftable last-commit ()
  ((repo :references (repository link) :primary-key t)
   (link :col-type (:varchar 256))
   (author :col-type (:varchar 128))
   (message :col-type (or :null :text))
   (time :col-type (or :null (:varchar 64)))))

(deftable commit ()
  ((link :col-type (:varchar 256) :primary-key t)
   (author :col-type (:varchar 128))
   (message :col-type (or :null :text))
   (time :col-type (or :null (:varchar 64)))
   (repo :references (repository link))))

(deftable pull-request ()
  ((link :col-type (:varchar 256) :primary-key t)
   (author :col-type (:varchar 128))
   (title :col-type (or :null (:varchar 256)))
   (message :col-type (or :null :text))
   (time :col-type (or :null (:varchar 64)))
   (repo :references (repository link))))

(deftable issue ()
  ((link :col-type (:varchar 256) :primary-key t)
   (author :col-type (:varchar 128))
   (title :col-type (or :null (:varchar 256)))
   (message :col-type (or :null :text))
   (time :col-type (or :null (:varchar 64)))
   (repo :references (repository link))))
