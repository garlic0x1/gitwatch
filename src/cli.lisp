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
   :name "add" :description "Add repo(s)"
   :handler (lambda (cmd)
              (->> (command-arguments cmd)
                (mapcar #'utils:make-repo)
                (mapcar #'mito:insert-dao)))))

(defvar repo/rm
  (make-command
   :name "rm" :description "Remove repo(s)"
   :handler (lambda (cmd) (mapcar (lambda (uri) (mito:delete-by-values 'db:repository :link uri))
                             (command-arguments cmd)))))

(defvar repo/add-user
  (make-command
   :name "add-user" :description "Add all public repos owned by user"
   :handler (lambda (cmd)
              (dolist (user (command-arguments cmd))
                (dolist (repo (scraper:user-repos user))
                  (ignore-errors (mito:insert-dao repo)))))))

(defvar repo/rm-user
  (make-command
   :name "rm-user" :description "Remove all repos owned by user(s)"
   :handler (lambda (cmd)
              (dolist (user (command-arguments cmd))
                (mito:delete-by-values 'db:repository :user user)))))

(defvar repo
  (make-command
   :name "repo" :description "Manipulate repositories"
   :sub-commands (list repo/ls repo/add repo/rm repo/add-user repo/rm-user)
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
              (dolist (repo (mito:select-dao 'db:repository))
                (let* ((new (scraper:last-commit repo))
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
                    ((and old (not same)) (progn (mailer:send new) (mito:update-dao new)))
                    ((not old) (mito:insert-dao new))))))))

;;
;; Top level command
;;

(defvar cli
  (make-command
   :name "git-notify"
   :sub-commands (list migrate repo scrape)
   :handler (lambda (cmd) (print-usage-and-exit cmd t))))
