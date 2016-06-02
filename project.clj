(defproject imap-contacts-cj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;;[imap-clj "0.6.4"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.xerial/sqlite-jdbc "3.7.2"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [io.forward/clojure-mail "1.0.4" ]
                 ]
  :main ^:skip-aot imap-contacts-cj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
