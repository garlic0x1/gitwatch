(asdf:defsystem "gitwatch"
  :depends-on (:binding-arrows
               :alexandria
               :dexador
               :clingon
               :quri :mito
               :str :trivia
               :yason :xmls)
  :components ((:module "config"
                :components ((:file "secrets")))
               (:module "src"
                :components ((:file "db")
                             (:file "utils")
                             (:file "mailer")
                             (:file "scraper")
                             (:file "cli")))))
