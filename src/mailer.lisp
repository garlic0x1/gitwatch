(defpackage mailer
  (:use :cl :alexandria-2 :db)
  (:export #:send))
(in-package :mailer)

;;
;; Parameters for discord client
;; (change hook)
;;

(defparameter discord-hook "https://discord.com/api/webhooks/1157233913882738729/4XtJzd8RYqY6GNnPVO18T3suZzvvwN3e0nmydiTFBsGE_tT0NIPd4bSzhIfxCLwBDGuK")
(defparameter discord-headers `(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/600.3.18 (KHTML, like Gecko) Version/8.0.3 Safari/600.3.18")
                                ("Content-Type" . "application/json")))
;;
;; Client helper functions
;;

(defun json-string (obj)
  (with-output-to-string (stream) (yason:encode obj stream)))

(defun embed-image (url)
  (alist-hash-table `(("image" . ,(alist-hash-table `(("url" . ,url)))))))

(defun discord-send (username message &key image)
  (dex:post discord-hook
   :headers discord-headers
   :content (json-string (alist-hash-table
                          `(("username" . ,username)
                            ("content" . ,message)
                            ,(when image `("embeds" . (,(embed-image image)))))))))

;;
;; Send a data model to discord
;;

(defgeneric send (obj)
  (:method ((obj commit))
    (discord-send
     (db::commit-author obj)
     (format nil "~a" (db::commit-link obj))))

  (:method ((obj issue))
    (discord-send
     (db::issue-author obj)
     (format nil "~a" (db::issue-link obj))))

  (:method ((obj pull-request))
    (discord-send
     (db::pull-request-author obj)
     (format nil "~a" (db::pull-request-link obj)))))
