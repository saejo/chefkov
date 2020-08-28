(defproject clj-bots "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [twitter-api "1.8.0"]
                 [enlive "1.1.6"]
                 [org.clojure/java.jdbc "0.7.11"]
                 [org.xerial/sqlite-jdbc "3.23.1"]]
  :repl-options {:init-ns clj-bots.twitter}
  #_#_:profiles {:uberjar {:aot :all}}
  :main clj-bots.twitter
  :uberjar-name "chefkov.jar")
