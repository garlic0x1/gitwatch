(asdf:defsystem "gitwatch"
  :depends-on (:binding-arrows
               :alexandria
               :cl-workers
               :dexador
               :clingon
               :cl-cron
               :trivia
               :yason
               :quri
               :mito
               :xmls
               :str)
  :components ((:module "config"
                :components ((:file "secrets")))
               (:module "src"
                :components ((:file "db")
                             (:file "utils")
                             (:file "mailer")
                             (:file "scraper")
                             (:file "cli")
                             (:file "core")))))
