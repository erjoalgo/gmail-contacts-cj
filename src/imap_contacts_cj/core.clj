(ns imap-contacts-cj.core
  (:require
   [clojure-mail.gmail :as gmail]
   [imap-contacts-cj.db :as db]
   [imap-contacts-cj.util :refer [crop-string read-password
                                  message-name-address-map-list]]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.tools.logging :as log]
   [clojure.java.io :as io ]
   )
  (:gen-class))

(def default-max-messages 600)

(def cli-options
  [["-e" "--email EMAIL" "email address"]
   ["-d" "--db DB" "path to sqlite db"
    :default (-> (clojure.java.io/file (System/getenv "HOME")
                                       ".imap-contacts.db")
                 .toString)]
   ["-m" "--max-results MAX"
    (format "max results to fetch, default %d, 0 for infinite"
            default-max-messages)
    :parse-fn #(Integer/parseInt %)
    :default default-max-messages]
   ["-p" "--passwd-file PASSWD_FN"
    "path to file containing app specific pass. prompt if not provided"]
   ["-n" "--newline" "flag to insert newlines instead of \\r" :default false]
   ["-q" "--quiet" "quiet" :default false]
   ["-s" "--imap-protocol-host-port IMAP_SERVER"
    (str "url for for imap server including protocol, host, port."
         "example 'https://imap.gmail.com:993'")
    :parse-fn #(let [match (re-matches
                            #"(https?)://([a-zA-Z0-9.]+):([0-9]+)" %)]
                  (if-not match
                    (throw (Exception. "invalid imap server string"))
                      (let [[_ protocol host port] match
                            port (Integer/parseInt port)]
                        [protocol host port])))
    :default ["https" "imap.gmail.com" 993]]
   ["-b" "--batch-size" "batch size per thread" :default 50]])

(defn message-uid
  "return the uid of the message"
  ([message] (message-uid (.getFolder message) message))
  ([folder message] (.getUID folder message)))

(defn message-name-address-map-list [message]
  ";=> ({:address \"ealfonso@cmu.edu\", :name \"my name\"}
{:address \"notification+bla-bla@facebookmail.com\", :name \"Facebook\"})"
  (reduce (fn [cum fun]
            (apply conj cum (fun message)))
          [clojure-mail.message/to
           clojure-mail.message/from
           clojure-mail.message/cc
           clojure-mail.message/bcc]))

(defn store-message-message-batch! [db folder-name message-batch]
  (let [name-address-map-lists (map message-name-address-map-list message-batch)
        uids (map message-uid message-batch)]
    ;;we want to minimize the number of db writes

    ;;TODO do both of the following in a single db transaction
    (db/insert-name-address-to-db! db name-address-map-lists)
    (db/store-uids! db uids)))

(defn update-progress! [batch-index batch-size total-message-count]
  (printf "\r%d/%d" (min (* batch-size (inc batch-index))
                         total-message-count)
          total-message-count)
  (flush))

(defn store-messages! [db folder-name messages batch-size
                      ;{:keys [batch-size]
                       ;:or {batch-size 50}}
                       ]
  (let [partitioned (partition batch-size batch-size nil messages)
        pipe! (pmap store-message-message-batch partitioned)
        total-message-count (count messages)]
    (doseq [[batch-index batch] (indexed pipe!)]
      (update-progress batch-index batch-size total-message-count))))

(def LAST-UID  javax.mail.UIDFolder/LASTUID)
(def FIRST-UID 1)

(defn messages-to-fetch! [db store folder-name]
  (let [folder (clojure-mail.core/open-folder store folder-name :readonly)]

    ;;if validity changed, drop all known uids
    (->> (.getUIDValidity inbox)
         (db/update-uid-validity-if-changed! db folder-name
                                             current-uid-validity))

    (let [[smallest-uid largest-uid] (db/smallest-largest-known-uids
                                      folder-name
                                      db)
          ;;newer than our newest message, most recent first
          newer-messages (.getMessagesByUID
                          inbox
                          (if largest-uid (inc largest-uid) FIRST) LAST)


          ;;older than our oldest message, most recent first
          older-messages (when (and smallest-uid (> smallest-uid FIRST))
                           (.getMessagesByUID inbox FIRST (dec smallest-uid)))

          newest (first newer-messages)
          newest-is-oldest (and newest (= largest-uid (message-uid newest)))]
      ;;we might have to remove oldest message, which is always returned
      (if newest-is-oldest
        (rest newer-messages) newer-messages))))


(defn fetch-and-store-new-messages! [db store folder-names
                                     {:keys [max-messages
                                             newline
                                             quiet
                                             batch-size]
                                      :or {folder-name "INBOX"}}]
  (doseq [folder-name folder-names]
    (log/infof "on folder %s" folder-name)
    (let [messages (messages-to-fetch! db store folder-name)]
      (store-messages! db folder-name messages batch-size))))


(defn process-messages [db store & {:keys [folder-names
                                           max-messages
                                           newline quiet batch-size]
                                    :or {folder-name "INBOX"}}]


  total-message-count (count messages)
  ;;make sure we're getting messages in ascending order
  (assert (or (-> messages nnext nil?)
              (->> (take 2 messages) (map message-uid) >)))

  (doseq [[batch-number batch]
          (indexed (pmap
                    ;;map over each batch in parallel
                    (partial map (juxt message-uid message-name-address-map-list))
                    (partition batch-size
                               batch-size
                               nil
                               (take (or max-messages total-message-count) messages))))]
    (printf "\r%d/%d" (min (* batch-size (inc batch-number)) total-message-count)
            total-message-count)
    (flush)
    (let [name-address-map-lists (flatten (map second batch))
          uids (map first batch)]
      (db/insert-name-address-to-db! db name-address-map-lists)
      (db/store-uids! db uids))))

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
            quiet (:quiet opts)
            batch-size (:batch-size opts)
            pass (if passwd-file (slurp passwd-file) (read-password :prompt "enter imap account password: "))

            db (db/sqlite-db-connection-for-file db-filename)

            mail-store (let [[protocol imap-host imap-port] (:imap-protocol-host-port opts)
                             protocol (case protocol
                                        "http" "imap"
                                        "https" "imaps")]
                         ;;gstore (clojure-mail.gmail/store email pass) ;;gmail-specific
                         ;(clojure-mail.core/store "imaps" ["imap.gmail.com" 993] email pass)
                         (clojure-mail.core/store protocol [imap-host imap-port] email pass))]

        ;;(db/ensure-tables-exist db/db :drop true)
        (db/ensure-tables-exist db)
        (process-messages db mail-store
                          :max-messages (when-not (= 0 max-results) max-results)
                          :newline newline
                          :batch-size batch-size
                          :quiet quiet)))))



;; Local Variables:
;; compile-command: "lein run -- -m 0 --db ~/.imap-contacts.db -e erjoalgo@gmail.com -p ~/repos/imap-contacts-cj/pass -s https://imap.gmail.com:993 -q"
;; End:
