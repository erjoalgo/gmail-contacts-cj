(ns imap-contacts-cj.db
  ;;(:require [clojure.java.jdbc :refer [query execute! insert!]])
  (:require [clojure.java.jdbc :as j]
            [clojure.tools.logging :as log]))

(defn ensure-tables-exist [db & {:keys [drop]}]
  (let [create-queries
        (if-not drop
          [
           "create table if not exists msguids (msguid integer not null primary key)"
           "create table if not exists uid_validity (uid_validity integer not null primary key)"
           "create table if not exists contacts (name text, address text not null unique)"
           ]

          ["drop table if exists msguids"
           "drop table if exists uid_validity"
           "drop table if exists contacts"])]
    (log/debugf "running queries: %s" create-queries )
    (dorun (map (partial j/execute! db) create-queries))))

(defn store-uid! [db uid]
  (j/insert! db "msguids" ["msguid"] [uid]))

(defn last-known-uid [db]
  ;;((keyword "max(msguid)") (first
  (-> db
      (j/query "select max(msguid) as max_msguid from msguids")
      first
      :max_msguid))

(defn last-uid-validity  [db]
  (:uid_validity
   (first
    (j/query db "select uid_validity from uid_validity"))))

;(sql/execute! db ["INSERT INTO fruit ( name, appearance, cost ) VALUES ( ?, ?, ? )"
                  ;"Apple" "Green" 75])

(defn update-uid-validity!  [db new-validity]
  "drop all known msguids, update uid_validity single row table"
  (j/execute! db "delete from uid_validity")
  (j/execute! db "delete from msguids")
  (j/execute! db ["insert into uid_validity values (?)" new-validity]))

(defn count-messages [db]
  (-> db (j/query "select count(msguid) as count_msguid from msguids")
      first :count_msguid))

(defn update-uid-validity-if-changed! [db current-validity]
  (let [last-validity (last-uid-validity db)]
    (when-not (= last-validity current-validity)
      (log/warnf "validity changed. need to drop all (%d) known message ids. (old, new) = (%d, %d)"
                 (count-messages db)
                 last-validity current-validity)
      (update-uid-validity! db current-validity)
      last-validity)))

(defn insert-name-address-to-db! [db name-address-map-list]
  (assert (every? :address name-address-map-list))
  (let [address-list (map :address  name-address-map-list)
        statement (apply vector
                         (str "insert or ignore into contacts (name, address)"
                              "values (?, ?)")
                         (map (juxt :name :address) name-address-map-list))]
    (j/execute! db statement {:multi? true})))

(defn sqlite-db-connection-for-file [db-filename]
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     db-filename})

