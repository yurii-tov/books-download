(defun run-books-download ()
  (interactive)
  (let ((default-directory "c:/Users/jurys/work/books-download"))
    (async-shell-command
     "ssh -Nv -L localhost:8910:localhost:8910 nexus"
     "*phantomjs-tunnel*")
    (async-shell-command
     "java -cp target/books-download-0.1.0-SNAPSHOT-standalone.jar clojure.main"
     "*books-download*")
    (with-current-buffer "*books-download*"
      (insert "(do (require 'books-download.core)
                   (in-ns 'books-download.core)
                   (require '[clojure.repl :refer [doc]])
                   (defn d [id file] (with-driver {:browser :phantomjs
                                                   :remote-url \"http://127.0.0.1:8910\"}
                        (download-book id file)))
                   (doc d))")
      (comint-send-input))))
