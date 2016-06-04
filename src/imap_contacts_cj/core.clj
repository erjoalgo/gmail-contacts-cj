(ns imap-contacts-cj.core
  (:require
   ;[clojure-mail]
   [clojure-mail.gmail :as gmail]
   ;[clojure-mail.core :refer :all]
   ;[clojure-mail.message :refer (read-message)]
   ;[clojure-mail.message :as message]
   [imap-contacts-cj.db :as db]
   [imap-contacts-cj.util :refer [crop-string read-password message-name-address-map-list]]
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.tools.logging :as log]
   [clojure.java.io :as io ]
   )
  (:gen-class))

(def default-max 600)

(def cli-options
  [["-e" "--email EMAIL" "email address"
    :default "erjoalgo@gmail.com"]
   ["-d" "--db DB" "path to sqlite db"
    :default (-> (clojure.java.io/file (System/getenv "HOME") ".imap-contacts.db") .toString)]
   ["-m" "--max-results MAX" (format "max results to fetch, default %d, 0 for infinite"
                                     default-max)
    :parse-fn #(Integer/parseInt %)
    :default default-max]
   ["-p" "--passwd-file PASSWD_FN" "path to file containing app specific pass. user is prompted if not provided"]
   ["-n" "--newline" "flag to insert newlines instead of \\r" :default false]
   ["-q" "--quiet" "quiet" :default false]
   ["-s" "--imap-protocol-host-port IMAP_SERVER"
    "url for for imap server including protocol, host, port, example 'https://imap.gmail.com:993'"
    :parse-fn #(let [match (re-matches #"(https?)://([a-z.]+):([0-9]+)" %)]
                  (if-not match
                    (throw (Exception. "invalid imap server string"))
                      (let [[_ protocol host port] match
                            port (Integer/parseInt port)]
                        [protocol host port])))
    :default ["https" "imap.gmail.com" 993]]])

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

(defn process-messages [db store & {:keys [folder-name max-messages newline quiet batch-size]
                                    :or {folder-name "INBOX"
                                         batch-size 100}}]
  (let [inbox (clojure-mail.core/open-folder store folder-name :readonly)]
    (let [current-uid-validity (.getUIDValidity inbox)]
      (db/update-uid-validity-if-changed! db current-uid-validity))

    (let [[smallest-uid largest-uid] (db/smallest-largest-known-uids db)
          FIRST 1
          LAST javax.mail.UIDFolder/LASTUID
          echo #(doto % println)
          messages (concat
                    ;;newer than our newest message, most recent first
                    (let [newer-messages (.getMessagesByUID inbox (if largest-uid (inc largest-uid) LAST) LAST)
                          newest (first newer-messages)]
                      ;;we might have to remove oldest message, which is always returned
                      (if (and newest largest-uid (= largest-uid (clojure-mail.message/uid newest)))
                        (rest newer-messages) newer-messages))

                    ;;older than our oldest message, most recent first
                    (when-not  (= smallest-uid 1)
                      (.getMessagesByUID inbox FIRST (if smallest-uid (dec smallest-uid) LAST))))
          total-message-count (count messages)]
      ;;make sure we're getting messages in ascending order
      (assert (or (-> messages nnext nil?)
                  (->> (take 2 messages) (map clojure-mail.message/uid) >)))

      (doseq [[batch-number batch]
              (indexed (pmap
                        ;;map over each batch in parallel
                        (partial map (juxt clojure-mail.message/uid message-name-address-map-list))
                        (partition batch-size
                                   batch-size
                                   nil
                                   (take (or max-messages total-message-count) messages))))]
        (printf "\r%d/%d" (* batch-size (inc batch-number)) total-message-count)
        (flush)
        (let [name-address-map-lists (flatten (map second batch))
              uids (map first batch)]
          (when-not  quiet (printf "flushing... %s\n" (pr-str uids)))
          (db/insert-name-address-to-db! db name-address-map-lists)
          (db/store-uids! db uids))))))

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
            pass (if passwd-file (slurp passwd-file) (read-password :prompt "enter app specific pass: "))

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
                          :quiet quiet)))))



;; Local Variables:
;; compile-command: "lein run -- -m 0 --db ~/.imap-contacts.db -e erjoalgo@gmail.com -p ~/repos/imap-contacts-cj/pass -s https://imap.gmail.com:993 -q"
;; End:
