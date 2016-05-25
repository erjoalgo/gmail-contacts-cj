(ns gmail-contacts-cj.sqlite
  (:require [clojure.java.jdbc :refer :all]))



(require 'clojure.java.jdbc)

(defn ensure-tables-exist [db & {:keys [drop]}]
  (let [create-queries
        (if-not drop
          ["create table if not exists msguids (msguid integer not null primary key)"
         "create table if not exists addresses (address text not null primary key)"
           "create table if not exists names (name text not null, address text not null, foreign key (address) references addresses(address))"]

          ["drop table msguids"
           "drop table addresses"
           "drop table names"])]
    (dorun (map (partial clojure.java.jdbc/execute! db) create-queries))))


(defn store-uid [db uid]
  (clojure.java.jdbc/insert! db "msguids" ["msguid"] [uid]))

(defn last-known-uid [db]
  ;;((keyword "max(msguid)") (first
  (:max_msguid (first
                            (clojure.java.jdbc/query db "select max(msguid) as max_msguid from msguids"))))

(defn insert-name-address-to-db [db name-address-map-list]
  (clojure.java.jdbc/insert-multi! db "names" name-address-map-list))


(def db-filename "gmail-contacts.db")
(def db {:classname   "org.sqlite.JDBC"
         :subprotocol "sqlite"
         :subname     db-filename})
