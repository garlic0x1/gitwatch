(defpackage :gitwatch/cli
  (:use :cl :alexandria-2 :binding-arrows)
  (:import-from :clingon
                #:print-usage-and-exit
                #:make-command
                #:command-arguments)
  (:export #:cli))
(in-package :gitwatch/cli)

;;
;; Repository CRD
;;

(defvar repo/ls
  (make-command
   :name "ls" :description "List repos"
   :handler (lambda (_cmd) (declare (ignore _cmd))
              (loop :for repo :in (mito:select-dao 'db:repository)
                    :for link := (db::repository-link repo)
                    :do (print link)))))

(defvar repo/add
  (make-command
   :name "add" :description "Add repos"
   :handler (lambda (cmd) (mapcar (lambda (uri) (-> uri (print) (utils:make-repo) (mito:insert-dao)))
                             (command-arguments cmd)))))

(defvar repo/rm
  (make-command
   :name "rm" :description "Remove repos"
   :handler (lambda (cmd) (mapcar (lambda (uri) (mito:delete-by-values 'db:repository :link uri))
                             (command-arguments cmd)))))

(defvar repo
  (make-command
   :name "repo" :description "Manipulate repositories"
   :sub-commands (list repo/ls repo/add repo/rm)
   :handler (lambda (cmd) (print-usage-and-exit cmd t))))

;;
;; Set up database
;;

(defvar migrate
  (make-command
   :name "migrate" :description "Set up database"
   :handler (lambda (_cmd) (declare (ignore _cmd)) (db:migrate))))

;;
;; Main procedure, call this with cron
;;

(defvar scrape
  (make-command
   :name "scrape" :description "Scrape repos and mail new commits"
   :handler (lambda (_cmd) (declare (ignore _cmd))
              (dolist (repo (mito:select-dao 'db:repository))
                (dolist (commit (scraper:commits repo))
                  (when-let ((new-commit (ignore-errors (mito:insert-dao commit))))
                    (mailer:send-commit new-commit)))))))

;;
;; Top level command
;;

(defvar cli
  (make-command
   :name "git-notify"
   :sub-commands (list migrate repo scrape)
   :handler (lambda (cmd) (print-usage-and-exit cmd t))))
