(ns books-download.core
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]
            [clojure.java.shell :as sh]
            [selenium.core :refer :all]))


(def site "http://loveread.me")


(def template "<!DOCTYPE html>
<html>
<head>
    <title>Page %s</title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
    <!--styles-->
    %s
</head>
<body>
  <div class=\"pageBook\">
    <div class=\"textBook\">
%s
    </div>
</div>
</body>
</html>")


(defn cut-site [x]
  (cstr/replace x (str site "/") ""))


(defn download-page [book-id page-id out-dir]
  (let [url (format "%s/read_book.php?id=%s&p=%s" site book-id page-id)
        out-file (io/file out-dir (format "%010d.html" page-id))]
    (open-url url)
    (doseq [x (find-elements (css "#content > div > div.textBook > div.rdImage, #content > div > div.textBook > h2, #content > div > div.textBook > div.numberPage, #content > div > div.textBook > form, #content > div > div.textBook > div.navigation, #content > div > div.textBook > div.navBlock"))]
      (execute-javascript "arguments[0].remove()" x))
    (let [page-element (find-element (css ".textBook"))
          images (map (fn [x] (element-attribute x "src"))
                      (find-elements page-element (css "img")))
          stylesheets (map (fn [x] (element-attribute x "href"))
                           (find-elements (css "link[rel=stylesheet]")))
          stylesheet-links (cstr/join "\n" (map (comp (partial format "<link type=\"text/css\" rel=\"stylesheet\" href=\"%s\">")
                                                      cut-site)
                                                stylesheets))
          page-raw (cut-site (element-inner-html page-element))]
      (doseq [xs [images stylesheets] x xs
              :let [f (io/file out-dir (cut-site x))]
              :when (not (.exists f))]
        (io/make-parents f)
        (with-open [s (io/input-stream x)]
          (io/copy s f)))
      (spit out-file (format template page-id stylesheet-links page-raw)))))


(defn convert-to-epub [book-dir out-file]
  (let [epub-temp-out-file (io/file book-dir "book.epub")]
    (doseq [x ["META-INF/container.xml"
               "content.opf"
               "mimetype"
               "toc.ncx"]
            :let [f (io/file book-dir x)
                  r (io/resource (str "epub_metadata/" (.getName f)))]]
      (io/make-parents f)
      (with-open [s (io/input-stream r)]
        (io/copy s f)))
    (let [pages (sort (for [x (file-seq book-dir)
                            :when (cstr/ends-with? x ".html")]
                        (cstr/replace (.getName x) ".html" "")))
          [a b] (for [s ["<item id=\"%1$s\" href=\"%1$s.html\" media-type=\"application/xhtml+xml\"/>"
                         "<itemref idref=\"%s\" linear=\"yes\"/>"]]
                  (->> pages
                       (map (partial format s))
                       (cstr/join "\n")))
          content (io/file book-dir "content.opf")]
      (as-> (slurp content) x
        (cstr/replace x "<!-- A -->" a)
        (cstr/replace x "<!-- B -->" b)
        (spit content x)))
    (sh/with-sh-dir book-dir (sh/sh "zip" "-qr" (.getName epub-temp-out-file) "."))
    (io/copy epub-temp-out-file (io/file out-file))
    (io/delete-file epub-temp-out-file)))


(defn download-book
  ([book-id pages-count out-file]
   (let [out-dir (io/file (str book-id "_" (System/currentTimeMillis)))]
     (io/make-parents (io/file out-dir "."))
     (println (format "Downloading book (%s pages)..." pages-count))
     (dotimes [i pages-count]
       (download-page book-id (inc i) out-dir))
     (println "Packing .epub...")
     (convert-to-epub out-dir out-file)
     (->> out-dir file-seq reverse (map (memfn delete)) dorun)))
  ([book-id out-file]
   (open-url (format "%s/read_book.php?id=%s&p=1" site book-id))
   (let [pages-count (->> (find-elements (css ".navigation a"))
                          (map (fn [x] (->> (element-attribute x "href")
                                            (re-find #"p=(\d+)")
                                            second
                                            read-string)))
                          (apply max))]
     (download-book book-id pages-count out-file))))
