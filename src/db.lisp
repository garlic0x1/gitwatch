(defpackage db
  (:use :cl :alexandria-2)
  (:export #:migrate #:repository #:last-commit #:commit #:issue #:pull-request))
(in-package :db)

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

;; one commit per repo
(deftable last-commit ()
  ((repo :references (repository link) :primary-key t)
   (link :col-type (:varchar 256))
   (author :col-type (:varchar 128))
   (message :col-type (or :null :text))
   (time :col-type (or :null (:varchar 64)))))

;;
;; Not using these yet
;;

;; (deftable commit ()
;;   ((link :col-type (:varchar 256) :primary-key t)
;;    (author :col-type (:varchar 128))
;;    (message :col-type (or :null :text))
;;    (time :col-type (or :null (:varchar 64)))
;;    (repo :references (repository link))))

;; (deftable pull-request ()
;;   ((link :col-type (:varchar 256) :primary-key t)
;;    (author :col-type (:varchar 128))
;;    (title :col-type (or :null (:varchar 256)))
;;    (message :col-type (or :null :text))
;;    (time :col-type (or :null (:varchar 64)))
;;    (repo :references (repository link))))

;; (deftable issue ()
;;   ((link :col-type (:varchar 256) :primary-key t)
;;    (author :col-type (:varchar 128))
;;    (title :col-type (or :null (:varchar 256)))
;;    (message :col-type (or :null :text))
;;    (time :col-type (or :null (:varchar 64)))
;;    (repo :references (repository link))))
