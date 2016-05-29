(ns smtp-contacts-cj.core
  (:require
   ;[clojure-mail]
   [clojure-mail.gmail :as gmail]
   ;[clojure-mail.core :refer :all]
   ;[clojure-mail.message :refer (read-message)]
   ;[clojure-mail.message :as message]
   [smtp-contacts-cj.db :as db]
   [smtp-contacts-cj.util :refer [crop-string read-password]]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.tools.logging :as log]
   )
  (:gen-class))

(defn message-name-address-map-list [message]
  ";=> ({:address \"ealfonso@cmu.edu\", :name \"my name\"} {:address \"notification+bla-bla@facebookmail.com\", :name \"Facebook\"})"
  (apply concat (map #(% message)
                     [clojure-mail.message/to
                      clojure-mail.message/from
                      clojure-mail.message/cc
                      clojure-mail.message/bcc])))

(def default-max 600)
(def cli-options
  [["-e" "--email EMAIL" "email address"
    :default "erjoalgo@gmail.com"]
   ["-d" "--db DB" "path to sqlite db"
    :default (format "%s/.smtp-contacts.db" (System/getenv "HOME"))]
   ["-m" "--max-results MAX" (format "max results to fetch, default %d, 0 for infinite"
                                     default-max)
    :parse-fn #(Integer/parseInt %)
    :default default-max]
   ["-p" "--passwd-file PASSWD_FN" "path to file containing app specific pass"]
   ["-n" "--newline" "flag to insert newlines instead of \\r" :default false]])



(defn process-messages [db store & {:keys [folder max-messages newline] :or {folder "INBOX"}}]
  (let [last-uid (db/last-known-uid db)
        messages (clojure-mail.core/all-messages store folder
                                                 :since-uid (and last-uid (+ 1 last-uid))
                                                 :oldest-first true)
        current-uid-validity (clojure-mail.core/get-folder-uid-validity
                              (clojure-mail.core/get-folder store folder))
        total-message-count (count messages)
        ]
    (log/debugf "current, last validity: %s, %s\n" current-uid-validity
                (db/last-uid-validity db))
    (db/update-uid-validity-if-changed! db current-uid-validity)
    (log/debugf "count: %d, first msg uid: %s, last uid: %s\n" (count messages)
                (clojure-mail.message/uid (first messages))
                last-uid)
    (assert (or (not (first messages))
                (empty? (rest messages))
                (not last-uid)
                (< last-uid (clojure-mail.message/uid (first messages)))))
    (loop [messages messages
           index 1]
      (when (and (first messages) (or (not max-messages) (< index max-messages)))
        (let [message (first messages)
              uid (clojure-mail.message/uid message)
              name-address-maps (message-name-address-map-list message)]
          (assert (not (= uid last-uid)))
          (do
            ;;TODO verbosity level
            (printf "%s%d/%d (uid %d) %s : '%s...' on %s"
                    (if newline "\n" "\r")
                    index total-message-count
                    uid
                    (:name (first (clojure-mail.message/from message)))
                    (crop-string 50 (clojure-mail.message/subject message))
                    (.format
                     (java.text.SimpleDateFormat. "E dd.MM.yyyy")
                     (clojure-mail.message/date-sent message)))
            (flush))
          (db/insert-name-address-to-db! db name-address-maps)
          (db/store-uid! db uid))
        (recur (rest messages) (+ 1 index))))))
  
    
(defn -main
  "fetch new mail, extact and store contacts"
  [& args]
  (let [args (parse-opts args cli-options)]
    (if (:errors args)
      (do (println (:summary args))
          (println (:errors args)))
      
      (let [opts (:options args)
            max-results (:max-results opts)
            db-filename (:db opts)
            email (:email opts)
            passwd-file (:passwd-file opts)
            newline (:newline opts)
            pass (if passwd-file (slurp passwd-file) (read-password :prompt "enter app specific pass: "))

            db (db/sqlite-db-connection-for-file db-filename)
            gstore (clojure-mail.gmail/store email pass)]
        
        ;;(db/ensure-tables-exist db/db :drop true)
        (db/ensure-tables-exist db)
        (process-messages db gstore
                          :max-messages (if-not (= 0 max-results) max-results)
                          :newline newline)))))



;;(def gstore (gmail/store "erjoalgo@gmail.com" (slurp "pass")))

;; Local Variables:
;; compile-command: "lein run -- -m 100 --db ~/.smtp-contacts.db -e erjoalgo@gmail.com"
;; End:
