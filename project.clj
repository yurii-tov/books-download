(defproject books-download "0.1.0-SNAPSHOT"
  :description "Books from website to epub"
  :repositories [["jitpack" "https://jitpack.io"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.github.yurii-tov/selenium "0.1.0-SNAPSHOT"]]
  :repl-options {:init-ns books-download.core})
