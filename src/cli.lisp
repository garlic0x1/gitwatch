(defpackage :gitwatch/cli
  (:use :cl :alexandria-2 :binding-arrows :mito)
  (:import-from #:mailer #:with-mailer)
  (:import-from :clingon #:print-usage-and-exit #:make-command #:command-arguments)
  (:export #:cli))
(in-package :gitwatch/cli)

;;
;; Repository CRD
;;

(defvar repo/ls
  (make-command
   :name "ls" :description "List repos"
   :handler (lambda (_cmd) (declare (ignore _cmd))
              (loop :for repo :in (select-dao 'db:repository)
                    :for link := (db::repository-link repo)
                    :do (format t "~a~%" link)))))

(defvar repo/add
  (make-command
   :name "add" :description "Add repo(s)"
   :handler (lambda (cmd)
              (->> (command-arguments cmd)
                (mapcar #'utils:make-repo)
                (mapcar #'insert-dao)))))

(defvar repo/rm
  (make-command
   :name "rm" :description "Remove repo(s)"
   :handler (lambda (cmd) (mapcar (lambda (uri) (delete-by-values 'db:repository :link uri))
                             (command-arguments cmd)))))

(defvar repo/add-user
  (make-command
   :name "add-user" :description "Add all public repos owned by user"
   :handler (lambda (cmd)
              (dolist (user (command-arguments cmd))
                (dolist (repo (scraper:user-repos user))
                  (ignore-errors (insert-dao repo)))))))

(defvar repo/rm-user
  (make-command
   :name "rm-user" :description "Remove all repos owned by user(s)"
   :handler (lambda (cmd)
              (dolist (user (command-arguments cmd))
                (delete-by-values 'db:repository :user user)))))

(defvar repo/add-file
  (make-command
   :name "add-file" :description "Add a list of URLs from a file"
   :handler (lambda (cmd)
              (->> (first (command-arguments cmd))
                (uiop:read-file-lines)
                (mapcar #'utils:make-repo)
                (mapcar #'insert-dao)))))

(defvar repo
  (make-command
   :name "repo" :description "Manipulate repositories"
   :sub-commands (list repo/ls repo/add repo/rm repo/add-user repo/rm-user repo/add-file)
   :handler (lambda (cmd) (print-usage-and-exit cmd t))))

;;
;; Set up database
;;

(defvar migrate
  (make-command
   :name "migrate" :description "Set up database (this will wipe any data you have accumulated)"
   :handler (lambda (_cmd) (declare (ignore _cmd)) (db:migrate))))

;;
;; Main procedure, call this with cron
;;

(defvar scrape
  (make-command
   :name "scrape" :description "Scrape repos and mail new commits"
   :handler (lambda (_cmd) (declare (ignore _cmd))
              (with-mailer
                (dolist (repo (select-dao 'db:repository))
                  (scraper:scrape-and-mail repo))))))

;;
;; Start as a service, as an alternative to cron
;;

(defvar service
  (make-command
   :name "service" :description "Start as a service that scrapes periodically"
   :handler (lambda (cmd)
              (with-mailer
                (cl-cron:make-cron-job
                 (lambda () (dolist (repo (select-dao 'db:repository))
                         (scraper:scrape-and-mail repo))))
                (bt:join-thread (cl-cron:start-cron))))))

;;
;; Top level command
;;

(defvar cli
  (make-command
   :name "gitwatch"
   :sub-commands (list migrate repo scrape service)
   :handler (lambda (cmd) (print-usage-and-exit cmd t))))
