(defpackage scraper
  (:use :cl :alexandria-2 :binding-arrows :utils)
  (:export #:commits))
(in-package :scraper)

(defun node-to-entry (repo node)
  (let* ((f (rcurry #'node-child node))
         (link (funcall f :link))
         (title (funcall f :title))
         (updated-at (funcall f :updated))
         (thumbnail (funcall f :thumbnail))
         (author (funcall f :author))
         (author-name (node-child :name author))
         (author-uri (node-child :uri author)))
    (mito:make-dao-instance 'db:commit
     :link (node-attr :href link)
     :message (node-text title)
     :date (node-text updated-at)
     :author (node-text author-name)
     :repo (db::repository-link repo))))

(defun commits (repo)
  (ignore-errors
   (->>
     (format nil "http://~a/~a/~a/commits.atom" (db::repository-host repo) (db::repository-user repo) (db::repository-repo repo) )
     (dex:get)
     (xmls:parse)
     (xmls:node-children)
     (remove-if-not (lambda (it) (string-equal :entry (xmls:node-name it))))
     (mapcar (curry #'node-to-entry repo)))))
