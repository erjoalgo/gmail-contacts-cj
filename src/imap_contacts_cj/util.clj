(ns imap-contacts-cj.util)

'(defn parse-message-date [date-str]
  "parse a date string"
  ;eg "Fri Sep 23 12:47:51 PDT 2011" (may be gmail specific)
  (.parse (java.text.SimpleDateFormat. "E MMMM d k:m:s z y") date-str))

(defn crop-string [max-chars string]
  (and string (.substring string 0 (min max-chars (.length string)))))

(defn read-password [ & {:keys [prompt] :or {prompt "Password:"}}]
  (assert (-> (System/console) nil? not))
  (String/valueOf (.readPassword (System/console) prompt nil)))

(defn echo-message [short total-message-count index message]
  "print some info about the message and return it"
  (if short
    (printf "\r%d/%d" index total-message-count)
  (printf "%s%d/%d %s : '%s...' on %s"
          (if newline "\n" "\r")
          index total-message-count
          (:name (first (clojure-mail.message/from message)))
          (crop-string 50 (clojure-mail.message/subject message))
          (.format
           (java.text.SimpleDateFormat. "E dd.MM.yyyy")
           (clojure-mail.message/date-sent message))))
  (flush)
  message)

;https://github.com/clojure/clojure-contrib/blob/b8d2743d3a89e13fc9deb2844ca2167b34aaa9b6/src/main/clojure/clojure/contrib/seq.clj#L51
(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.
  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))
