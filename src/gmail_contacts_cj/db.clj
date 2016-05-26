(ns gmail-contacts-cj.db
  ;;(:require [clojure.java.jdbc :refer [query execute! insert!]])
  (:require [clojure.java.jdbc :as j]
            [clojure.tools.logging :as log]))



(defn ensure-tables-exist [db & {:keys [drop]}]
  (let [create-queries
        (if-not drop
          [
           "create table if not exists msguids (msguid integer not null primary key)"
           "create table if not exists uid_validity (uid_validity integer not null primary key)"
           "create table if not exists addresses (address text not null primary key)"
           "create table if not exists names (name text not null, address text not null, foreign key (address) references addresses(address))"
           ]

          ["drop table if exists msguids"
           "drop table if exists uid_validity"
           "drop table if exists addresses"
           "drop table if exists names"])]
    (log/debugf "running queries: %s" create-queries )
    (dorun (map (partial j/execute! db) create-queries))))


(defn store-uid! [db uid]
  (j/insert! db "msguids" ["msguid"] [uid]))

(defn last-known-uid [db]
  ;;((keyword "max(msguid)") (first
  (:max_msguid (first
                (j/query db "select max(msguid) as max_msguid from msguids"))))

(defmacro unless [test & body]
  `(when (not ~test) ~@body))

(defn last-uid-validity  [db]
  (:uid_validity
   (first
    (j/query db "select uid_validity from uid_validity"))))

(defn update-uid-validity!  [db new-validity]
  "drop all known msguids, update uid_validity single row table"
  (j/execute! db
           "delete from uid_validity; delete from msguids; insert into uid_validity values (?);"
           new-validity))

(defn update-uid-validity-if-changed! [db current-validity]
  (let [last-validity (last-uid-validity db)]
    (unless (= last-validity current-validity)
            (update-uid-validity! db current-validity)
            last-validity)))

(defn insert-name-address-to-db! [db name-address-map-list]
  (assert (every? :address name-address-map-list))
  (let [address-list ((comp vec map) (comp vec :address)  name-address-map-list)]
    (j/execute! db
                "insert or ignore into addresses values (?)"
                address-list))
  
  (j/insert-multi! db "names"
                   ;;name can't be null for sql
                   (filter :name name-address-map-list)))

(def db-filename "gmail-contacts.db")
(def db {:classname   "org.sqlite.JDBC"
         :subprotocol "sqlite"
         :subname     db-filename})
