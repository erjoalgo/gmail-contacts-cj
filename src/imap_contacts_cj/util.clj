(ns imap-contacts-cj.util)
  
'(defn parse-message-date [date-str]
  "parse a date string"
  ;eg "Fri Sep 23 12:47:51 PDT 2011" (may be gmail specific)
  (.parse (java.text.SimpleDateFormat. "E MMMM d k:m:s z y") date-str))

(defn crop-string [max-chars string]
  (and string (.substring string 0 (min max-chars (.length string)))))

(defn read-password [ & {:keys [prompt] :or {prompt "Password:"}}]
  (String/valueOf (.readPassword (System/console) prompt nil)))

(defn message-name-address-map-list [message]
  ";=> ({:address \"ealfonso@cmu.edu\", :name \"my name\"} {:address \"notification+bla-bla@facebookmail.com\", :name \"Facebook\"})"
  (apply concat (map #(% message)
                     [clojure-mail.message/to
                      clojure-mail.message/from
                      clojure-mail.message/cc
                      clojure-mail.message/bcc])))
