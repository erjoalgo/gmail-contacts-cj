(defvar smtp-contacts-db-fn (f-expand "~/.smtp-contacts.db"))

(defvar smtp-standalone-jar-path (f-expand "~/bin/smtp-contacts-cj-0.1.0-SNAPSHOT-standalone.jar"))

(defun smtp-contacts-insert-contact ()
  (interactive)
  (insert (smtp-contacts-completing-read)))

(defun smtp-contacts-completing-read ()
  (completing-read "start entering desired  address or name: "
		   (smtp-contacts-get-contacts) nil t))

(defun smtp-contacts-get-contacts ()
  (let ((lines (s-split "\n"
		   (shell-command-to-string
		    (format "sqlite3 -line %s 'select * from addresses;'" smtp-contacts-db-fn)) t)))
    (mapcar (lambda (line) (subseq line (length "address = ")))
	  lines)))

(defun smtp-contacts-refresh ()
  (interactive)
  ;;"java -jar smtp-contacts-cj-0.1.0-standalone.jar [args]"
  (start-process "smtp-contacts-refresh"
		 "*smtp-contacts-refresh*"
		 "java" "-jar" smtp-standalone-jar-path
		 "-m" "200"
		 "--db" smtp-contacts-db-fn))

(add-hook 'gnus-summary-mode-hook 'smtp-contacts-refresh)

(with-eval-after-load "message"
  (define-key message-mode-map (kbd "\C-ci") 'smtp-contacts-insert-contact))

