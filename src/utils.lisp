(defpackage utils
  (:use :cl :alexandria-2 :binding-arrows :xmls)
  (:export #:node-text #:node-child #:node-attr
           #:make-repo
           #:user-repos))
(in-package :utils)

;;
;; Helpers for querying atom XML documents
;;

(defun node-text (node)
  (if (node-p node)
      (apply (curry #'concatenate 'string)
             (mapcar #'node-text (node-children node)))
      (format nil "~a" node)))

(defun node-child (name node)
  (->>
    (node-children node)
    (find-if (lambda (child) (string-equal name (node-name child))))))

(defun node-attr (attr node)
  (let ((val (assoc-value (node-attrs node) attr :test #'string-equal)))
    (if (= 1 (length val))
        (car val)
        val)))

;;
;; Model helpers
;;

(defun make-repo (uri)
  (let* ((quri (quri:uri uri))
         (host (quri:uri-host quri))
         (path (str:split "/" (quri:uri-path quri) :omit-nulls t))
         (user (first path))
         (repo (first (str:split "." (second path) :omit-nulls t))))
    (mito:make-dao-instance 'db:repository :link uri :host host :user user :repo repo)))

;;
;; Shell out to `gh` to list repos of user
;; Easier to manage API tokens this way
;;

(defun user-repos (username)
  (-<>
    (format nil "gh api users/~a/repos" username)
    (uiop:run-program <> :output :string)
    (yason:parse <>)
    (mapcar (curry #'gethash "clone_url") <>)
    (mapcar #'make-repo <>)))

