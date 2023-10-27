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

(defvar repo/add-user
  (make-command
   :name "add-user" :description "Add all public repos owned by user"
   :handler (lambda (cmd)
              (dolist (user (command-arguments cmd))
                (dolist (repo (utils:user-repos user))
                  (ignore-errors (mito:insert-dao repo)))))))

(defvar repo/rm-user
  (make-command
   :name "rm-user" :description "Remove all repos owned by user(s)"
   :handler (lambda (cmd)
              (dolist (user (command-arguments cmd))
                (mito:delete-by-values 'db:repository :user user)))))

(defvar repo/rm
  (make-command
   :name "rm" :description "Remove repo(s)"
   :handler (lambda (cmd) (mapcar (lambda (uri) (mito:delete-by-values 'db:repository :link uri))
                             (command-arguments cmd)))))

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
                (let* ((new (scraper:last-commit repo))
                       (old (mito:find-dao 'db:last-commit :repo (db::repository-link repo)))
                       (same (and old (string= (db::commit-link new) (db::commit-link old)))))

                  (format t "new: ~a~%old: ~a~%same: ~a~%" new old same)

                  ;;       old  same
                  ;; nop    t     t
                  ;; send   t     f
                  ;; send   f     t
                  ;; send   f     f


                  (when (not (and old same))
                    (mailer:send new)
                    (if old
                        (mito:update-dao new)
                        (mito:insert-dao new)))


                  ;; (mailer:send
                  ;;  (ignore-errors
                  ;;   (mito:insert-dao
                  ;;    (scraper:last-commit repo))))
                  )))))

;;
;; Top level command
;;

(defvar cli
  (make-command
   :name "git-notify"
   :sub-commands (list migrate repo scrape)
   :handler (lambda (cmd) (print-usage-and-exit cmd t))))
