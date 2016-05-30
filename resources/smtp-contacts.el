(defvar smtp-contacts-db-fn (f-expand "~/.smtp-contacts.db"))

(defvar smtp-standalone-jar-path
  (f-expand "~/bin/smtp-contacts-cj-0.1.0-SNAPSHOT-standalone.jar"))

(defun smtp-contacts-insert-contact ()
  (interactive)
  (insert (smtp-contacts-name-or-address-completing-read)))

(defun smtp-addresses-completing-read ()
  (completing-read "start entering desired  address or name: "
		   (smtp-contacts-get-addresses) nil t))

(defun smtp-contacts-get-addresses ()
  (mapcar 'cdar (sqlite3-query smtp-contacts-db-fn "select address from addresses")))

(defun smtp-contacts-name-or-address-completing-read ()
  "complete by either name or address"
  (let* ((records (sqlite3-query
			     smtp-contacts-db-fn
			     "select name, address from contacts"))
	(names-emails-alist (mapcar (lambda (record) (mapcar 'cdr record))
				    records))
	name-or-email)
    
    (setf name-or-email (completing-read
			 "completing read of address or name: "
			 names-emails-alist nil t))
    (let ((cell (assoc-string name-or-email names-emails-alist t)))
	  (or (cadr cell) name-or-email))))
    

(defun sqlite3-query (fn query)
  "returns an alist (col-name . col-value) for each record. example
((\"name\" . \"Ernesto Alfonso\")
 (\"address\" . \"erjoalgo@gmail.com\"))"
  (let ((out (shell-command-to-string
	      (format "sqlite3 -line %s \"%s\"" fn query))))
    (loop with start = 0
	  while (string-match "\\(^[[:space:]]*\\([a-z]+\\) = \\(.*\\)\n\\)+" out start)
	  do (setf start (match-end 0))
	  collect (mapcar (lambda (col-val) (cons (second col-val) (third col-val)))
			  (s-match-strings-all
			   "^[[:space:]]*\\([a-z]+\\) = \\(.*\\)" (match-string 0 out))))))

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

