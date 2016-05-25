(ns gmail-contacts-cj.core
  (:require
   [clojure-mail.core :refer :all]
   [clojure-mail.gmail :as gmail]
   [clojure-mail.message :refer (read-message)]
   [gmail-contacts-cj.sqlite :as sqlite]
            ;[gmail-clj.core :as gmail-clj]
   )
  (:gen-class))



(def gstore (gmail/store "erjoalgo@gmail.com" "PASSWORD" )) 

(defn message-name-address-map-list [message]
  ";=> ({:address \"ealfonso@cmu.edu\", :name \"my name\"} {:address \"notification+bla-bla@facebookmail.com\", :name \"Facebook\"})"
  (apply concat (map #(% message) '(:to :from :cc :bcc))))


(defn process-messages [db store & {:keys [folder] :or {folder "INBOX"}}]
  (let [last-uid (sqlite/last-known-uid db)
        ;;_ (printf "last known: %s\n" last-uid)
        messages (clojure-mail.core/all-messages store folder :since-uid (and last-uid (+ 1 last-uid)))
        index 0]
    (loop [messages messages
           index 1]
      (when (first messages)
        (let [message (first messages)
              uid (clojure-mail.message/uid message)
              name-address-maps (message-name-address-map-list message)]

          (do
            (printf "\ron message %d (uid: %d)" index uid)
            (flush))

          (sqlite/insert-name-address-to-db db name-address-maps)
          (sqlite/store-uid db uid)))
      (recur (rest messages) (+ 1 index)))))

  
(defn -main
  "fetch new mail, extact and store contacts"
  [& args]
  (sqlite/ensure-tables-exist sqlite/db)
  (process-messages sqlite/db gstore))
