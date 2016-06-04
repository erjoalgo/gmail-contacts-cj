;;; imap-contacts.el --- 

;; Copyright (C) 2016  Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:



(defvar imap-contacts-db-fn (f-expand "~/.imap-contacts.db"))

(defvar imap-contacts-standalone-jar-path
  (f-expand "~/bin/imap-contacts-cj-0.1.0-SNAPSHOT-standalone.jar"))

(defun imap-contacts-insert-contact ()
  (interactive)
  (insert (imap-contacts-name-or-address-completing-read)))

(defun imap-addresses-completing-read ()
  (completing-read "start entering desired  address or name: "
		   (imap-contacts-get-addresses) nil t))

(defun imap-contacts-get-addresses ()
  (mapcar 'cdar (sqlite3-query imap-contacts-db-fn "select address from addresses")))

(defun imap-contacts-name-or-address-completing-read ()
  "complete by either name or address"
  (let* ((records (sqlite3-query
			     imap-contacts-db-fn
			     "select name, address from contacts"))
	 ;;this works because name comes first, so (name . address)
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
					;don't use [:space:], it is buffer-specific and may cause a crash
	  while (string-match "\\(^[ ]*\\([a-z]+\\) = \\(.*\\)\n\\)+" out start)
	  do (setf start (match-end 0))
	  collect (mapcar (lambda (col-val) (cons (second col-val) (third col-val)))
			  (s-match-strings-all
			   "^[ ]*\\([a-z]+\\) = \\(.*\\)" (match-string 0 out))))))

(defun imap-contacts-refresh ()
  (interactive)
  ;;"java -jar imap-contacts-cj-0.1.0-standalone.jar [args]"
  (start-process "imap-contacts-refresh"
		 "*imap-contacts-refresh*"
		 "java" "-jar" imap-standalone-jar-path
		 "-m" "200"
		 "--db" imap-contacts-db-fn))

(add-hook 'gnus-summary-mode-hook 'imap-contacts-refresh)

(with-eval-after-load "message"
  (define-key message-mode-map (kbd "\C-ci") 'imap-contacts-insert-contact))

(provide 'imap-contacts)
;;; imap-contacts.el ends here


