(defpackage scraper
  (:use :cl :alexandria-2 :binding-arrows :utils)
  (:export #:commits #:last-commit #:user-repos #:scrape-and-mail))
(in-package :scraper)

;;
;; Functions to transform XML nodes into DB models
;;

(defun node->commit* (table repo node)
  (let* ((f (rcurry #'node-child node))
         (link (funcall f :link))
         (title (funcall f :title))
         (updated-at (funcall f :updated))
         ;; (thumbnail (funcall f :thumbnail))
         (author (funcall f :author))
         (author-name (node-child :name author))
         ;; (author-uri (node-child :uri author))
         )
    (mito:make-dao-instance
     table
     :link (node-attr :href link)
     :message (node-text title)
     :time (node-text updated-at)
     :author (node-text author-name)
     :repo (db::repository-link repo))))

(defun node->commit (repo node) (node->commit* 'db:commit repo node))

(defun node->last-commit (repo node) (node->commit* 'db:last-commit repo node))

;;
;; Scrape /commits.atom feed to get latest commits in a repo
;;

(defun commits (repo)
  (ignore-errors
   (->>
     (format nil "https://~a/~a/~a/commits.atom" (db::repository-host repo) (db::repository-user repo) (db::repository-repo repo) )
     (dex:get)
     (xmls:parse)
     (xmls:node-children)
     (remove-if-not (lambda (it) (string-equal :entry (xmls:node-name it))))
     (mapcar (curry #'node->commit repo)))))

(defun last-commit (repo)
  (ignore-errors
   (->>
     (format nil "https://~a/~a/~a/commits.atom" (db::repository-host repo) (db::repository-user repo) (db::repository-repo repo) )
     (dex:get)
     (xmls:parse)
     (xmls:node-children)
     (find-if (lambda (it) (string-equal :entry (xmls:node-name it))))
     (node->last-commit repo))))

;;
;; Shell out to `gh` to list repos of user
;; It is easier to manage API tokens this way
;;

(defun user-repos (username)
  (-<>
    (format nil "gh api users/~a/repos" username)
    (uiop:run-program <> :output :string)
    (yason:parse <>)
    (mapcar (curry #'gethash "clone_url") <>)
    (mapcar #'make-repo <>)))

;;
;; High level scraping/mailing operation
;;

(defun scrape-and-mail (repo)
  (let* ((new (last-commit repo))
         (old (mito:find-dao 'db:last-commit :repo (db::repository-link repo)))
         (same (and old (string= (db::last-commit-link new) (db::last-commit-link old)))))

    ;; cond table:
    ;;
    ;; input: | old  | same
    ;; nop    |  t   |   t
    ;; send   |  t   |   f
    ;; nop    |  f   |   t
    ;; nop    |  f   |   f

    (cond
      ((and old (not same)) (mailer:send new) (mito:update-dao new))
      ((and new (not old)) (mito:insert-dao new)))))
