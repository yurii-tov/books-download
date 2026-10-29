(ns books-download.toc-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as cstr]
            [books-download.toc :as toc]))


(defn- tmp-dir
  "A fresh directory under target/ for tests that need real files."
  []
  (let [d (io/file "target" (str "toc-test-" (System/nanoTime)))]
    (.mkdirs d)
    d))


(defn- write-page [dir name html]
  (spit (io/file dir name) html))


(def ^:private chapter-html
  (str "<html><body>"
       "<a name=\"gl_1\"></a><div class=\"take_h1\">Chapter &amp; One</div>"
       "<p class=\"MsoNormal\">text</p>"
       "<a name=\"gl_2\"></a>\n<div class=\"take_h1 extra\">Chapter <i>Two</i></div>"
       "</body></html>"))


(deftest html-toc-basics
  (testing "chapters are found in order with their anchors, titles and page"
    (is (= [{:level 1 :title "Chapter & One" :anchor "gl_1" :page "0000000001"}
            {:level 1 :title "Chapter Two" :anchor "gl_2" :page "0000000001"}]
           (toc/html-toc chapter-html "0000000001"))))

  (testing "no chapter headings means an empty table of contents"
    (is (= [] (toc/html-toc "<html><body><p>no headings here</p></body></html>" "p"))))

  (testing "a heading without a preceding anchor still shows up"
    (is (= [{:level 1 :title "Loose" :anchor nil :page "p"}]
           (toc/html-toc "<div class='take_h1'>Loose</div>" "p"))))

  (testing "take_hN sets the depth"
    (is (= [1 2 2 1]
           (map :level
                (toc/html-toc (str "<div class=\"take_h1\">A</div>"
                                   "<div class=\"take_h2\">A.1</div>"
                                   "<div class=\"take_h2\">A.2</div>"
                                   "<div class=\"take_h1\">B</div>")
                              "p")))))

  (testing "ids are accepted as well as names, and headings may be any block tag"
    (is (= [{:level 1 :title "Part I" :anchor "gl_4" :page "p"}]
           (toc/html-toc "<a id=\"gl_4\"></a><p class=\"take_h1\">Part I</p>" "p")))))


(deftest entities-and-tags
  (testing "tags are dropped and entities decoded"
    (is (= "« Тише » & <тише>"
           (toc/strip-tags "&laquo;<b>Тише</b>&raquo;&nbsp;&amp; &lt;тише&gt;")))
    (is (= "A — B" (toc/strip-tags "A &#8212; B")))
    (is (= "A — B" (toc/strip-tags "A &#x2014; B")))
    (is (= "&bogus; untouched" (toc/strip-tags "&bogus; untouched"))))

  (testing "escaped text is safe in xml"
    (is (= "A &amp; B &lt;c&gt; &quot;d&quot; &apos;e&apos;"
           (toc/escape-xml "A & B <c> \"d\" 'e'")))))


(deftest directory-toc
  (let [dir (tmp-dir)]
    ;; written out of order on purpose
    (write-page dir "0000000002.html" "<div class=\"take_h1\">Second</div>")
    (write-page dir "0000000001.html" chapter-html)
    (is (= ["Chapter & One" "Chapter Two" "Second"]
           (map :title (toc/extract-toc dir))))
    (is (= ["0000000001" "0000000001" "0000000002"]
           (map :page (toc/extract-toc dir))))
    (testing "a single page and a html string work too"
      (is (= 2 (count (toc/extract-toc (io/file dir "0000000001.html")))))
      (is (= ["Chapter & One" "Chapter Two"]
             (map :title (toc/extract-toc chapter-html)))))))


(def ^:private nested-toc
  [{:level 1 :title "A" :page "0000000001" :anchor "gl_1"}
   {:level 2 :title "A.1" :page "0000000001" :anchor "gl_2"}
   {:level 2 :title "A.2" :page "0000000002" :anchor nil}
   {:level 1 :title "B" :page "0000000003" :anchor "gl_9"}])


(deftest ncx-rendering
  (let [xml (toc/ncx-nav-map nested-toc)]
    (testing "one navPoint per chapter, numbered in document order"
      (is (= [1 2 3 4]
             (map (comp read-string second)
                  (re-seq #"playOrder=\"(\d+)\"" xml)))))
    (testing "entry targets"
      (is (cstr/includes? xml "<content src=\"0000000001.html#gl_1\"/>"))
      (is (cstr/includes? xml "<content src=\"0000000002.html\"/>"))
      (is (cstr/includes? xml "<content src=\"0000000003.html#gl_9\"/>")))
    (testing "sub-chapters are nested inside their chapter"
      (is (cstr/includes? xml (str "        <navPoint id=\"navPoint-1\" playOrder=\"1\">\n"
                                   "            <navLabel>\n"
                                   "                <text>A</text>\n"
                                   "            </navLabel>\n"
                                   "            <content src=\"0000000001.html#gl_1\"/>\n"
                                   "            <navPoint id=\"navPoint-2\" playOrder=\"2\">\n")))
      (is (cstr/includes? xml (str "        <navPoint id=\"navPoint-4\" playOrder=\"4\">")))))

  (testing "titles are xml-escaped"
    (is (cstr/includes? (toc/ncx-nav-map [{:level 1 :title "A & B" :page "p"}])
                        "<text>A &amp; B</text>")))

  (testing "an empty table of contents degrades to a single placeholder entry"
    (let [xml (toc/ncx-nav-map [] "0000000007")]
      (is (= 1 (count (re-seq #"<navPoint " xml))))
      (is (cstr/includes? xml "<content src=\"0000000007.html\"/>")))))


(deftest write-ncx-fills-the-template
  (let [dir (tmp-dir)]
    (write-page dir "0000000001.html" chapter-html)
    (spit (io/file dir "toc.ncx")
          (str "<ncx><navMap>\n        <!-- C -->\n    </navMap></ncx>"))
    (let [toc (toc/write-ncx dir)
          xml (slurp (io/file dir "toc.ncx"))]
      (is (= 2 (count toc)))
      (is (not (cstr/includes? xml "<!-- C -->")))
      (is (cstr/includes? xml "        <navPoint id=\"navPoint-1\" playOrder=\"1\">"))
      (is (cstr/includes? xml "<text>Chapter &amp; One</text>"))
      (is (cstr/includes? xml "    </navMap></ncx>"))))

  (testing "an ncx without the placeholder gets its navMap body replaced"
    (let [dir (tmp-dir)]
      (write-page dir "0000000001.html" chapter-html)
      (spit (io/file dir "toc.ncx")
            (str "<ncx><navMap>\n        <navPoint id=\"0000000001\" playOrder=\"1\">"
                 "<content src=\"0000000001.html\"/></navPoint>\n    </navMap></ncx>"))
      (let [xml (slurp (io/file dir "toc.ncx"))]  ;; before
        (is (cstr/includes? xml "0000000001.html"))
        (let [_ (toc/write-ncx dir)
              xml (slurp (io/file dir "toc.ncx"))]
          (is (= 2 (count (re-seq #"<navPoint " xml))))
          (is (not (cstr/includes? xml "id=\"0000000001\""))))))))


;;; --- markup variants seen on the live site --------------------------------

(deftest live-markup
  (testing "current markup: heading carries the anchor as its own id"
    (is (= [{:level 1 :title "Часть I Красный гаолян" :anchor "chapter-1" :page "0000000001"}
            {:level 1 :title "1" :anchor "chapter-2" :page "0000000001"}]
           (toc/html-toc (str "<div id=\"chapter-1\" class=\"book-title\">"
                              "<p class=\"book-title-line\">Часть\u00a0I</p>"
                              "<p class=\"book-title-line\">Красный гаолян</p>"
                              "</div>"
                              "<p class=\"book-paragraph\">text</p>"
                              "<div id=\"chapter-2\" class=\"book-title\">"
                              "<p class=\"book-title-line\">1</p>"
                              "</div>")
                         "0000000001"))))

  (testing "older books: name anchor in front of a book-title div"
    (is (= [{:level 1 :title "I" :anchor "gl_1" :page "p"}
            {:level 1 :title "II" :anchor "gl_2" :page "p"}]
           (toc/html-toc (str "<a name=\"gl_1\"></a>\n<div class=\"book-title\">I</div>\n"
                              "<p class=\"book-paragraph\">text</p>\n"
                              "<a name=\"gl_2\"></a>\n<div class=\"book-title\">II</div>")
                         "p"))))

  (testing "decorative classes are not chapters"
    (is (= [] (toc/html-toc (str "<h2 class=\"h2-title\">Книга такая-то</h2>"
                                 "<p class=\"book-section-subtitle\">* * *</p>"
                                 "<p class=\"book-title-line\">не глава</p>"
                                 "<div class=\"book-image\"><img src=\"x\"></div>")
                            "p"))))

  (testing "an anchor from an earlier chapter is not reused"
    (is (= [{:level 1 :title "I" :anchor "gl_1" :page "p"}
            {:level 1 :title "Loose" :anchor nil :page "p"}]
           (toc/html-toc (str "<a name=\"gl_1\"></a><div class=\"book-title\">I</div>"
                              "<p class=\"book-paragraph\">"
                              "a long paragraph that keeps the next heading away"
                              " from the anchor above</p>"
                              "<div class=\"book-title\">Loose</div>")
                         "p")))))


(deftest site-contents-page
  (let [html (str "<div class=\"indoorUnit\"><div class=\"capBlock\"><h2>Оглавление книги</h2></div>"
                  "<div class=\"bodyUnit\"><ul class=\"book-chapters\">\n"
                  "<li><a href=\"read_book.php?id=15497&p=1#gl_1\">I</a></li>\n"
                  "<li><a href=\"read_book.php?id=15497&p=2#gl_3\">Часть &laquo;вторая&raquo;</a></li>\n"
                  "<li><a href=\"read_book.php?id=15497&p=10#chapter-4\">IV</a></li>\n"
                  "</ul></div></div>")]
    (testing "chapters, titles, page numbers (as file names) and anchors"
      (is (= [{:level 1 :title "I" :page "0000000001" :anchor "gl_1"}
              {:level 1 :title "Часть «вторая»" :page "0000000002" :anchor "gl_3"}
              {:level 1 :title "IV" :page "0000000010" :anchor "chapter-4"}]
             (toc/contents-toc html))))

    (testing "a book without a contents page yields nil, not []"
      (is (nil? (toc/contents-toc "<html><body>no contents here</body></html>"))))

    (testing "an empty list is no table of contents either"
      (is (nil? (toc/contents-toc "<ul class=\"book-chapters\"></ul>"))))))


(deftest write-ncx-uses-the-given-toc
  (let [dir (tmp-dir)]
    (write-page dir "0000000001.html" chapter-html)
    (spit (io/file dir "toc.ncx") "<ncx><navMap><!-- C --></navMap></ncx>")
    (let [site-toc [{:level 1 :title "Часть первая" :page "0000000001" :anchor "gl_1"}
                    {:level 1 :title "Глава 2" :page "0000000002" :anchor "chapter-2"}]
          written (toc/write-ncx dir site-toc)
          xml (slurp (io/file dir "toc.ncx"))]
      (testing "the site's titles win over the scanned ones"
        (is (= site-toc written))
        (is (cstr/includes? xml "<text>Часть первая</text>"))
        (is (cstr/includes? xml "<content src=\"0000000002.html#chapter-2\"/>"))
        (is (not (cstr/includes? xml "Chapter &amp; One")))))
    (testing "an empty toc falls back to scanning the pages"
      (let [xml (do (toc/write-ncx dir []) (slurp (io/file dir "toc.ncx")))]
        (is (cstr/includes? xml "<text>Chapter &amp; One</text>"))))))

(defn- dom-contents
  "Chapter links wrapped the way contents.php wraps them."
  [links]
  (str "<ul class=\"book-chapters\">" links "</ul>"))


(deftest contents-page-as-the-browser-serialises-it
  (testing "the scraper sees the serialised DOM, where & in hrefs is &amp;"
    (is (= [{:level 1 :title "Предисловие" :page "0000000001" :anchor "gl_1"}
            {:level 1 :title "Глава 1. Страна" :page "0000000012" :anchor "gl_2"}]
           (toc/contents-toc
            (dom-contents (str "<li><a href=\"read_book.php?id=45501&amp;p=1#gl_1\">Предисловие</a></li>"
                               "<li><a href=\"read_book.php?id=45501&amp;p=12#gl_2\">Глава 1. Страна</a></li>"))))))

  (testing "a list without page numbers is no table of contents at all"
    (is (nil? (toc/contents-toc
               (dom-contents "<li><a href=\"read_book.php?id=1#gl_1\">I</a></li>")))))

  (testing "every rendered entry carries a file name"
    (let [xml (toc/ncx-nav-map
               (toc/contents-toc
                (dom-contents "<li><a href=\"read_book.php?id=1&amp;p=7#gl_9\">I</a></li>")))]
      (is (not (cstr/includes? xml "src=\"#")))
      (is (cstr/includes? xml "<content src=\"0000000007.html#gl_9\"/>")))))


(deftest unusable-contents-means-scanning-the-pages
  (let [dir (tmp-dir)
        toc (toc/contents-toc
             (dom-contents "<li><a href=\"read_book.php?id=1#gl_1\">I</a></li>"))]
    (write-page dir "0000000001.html" chapter-html)
    (spit (io/file dir "toc.ncx") "<ncx><navMap><!-- C --></navMap></ncx>")
    (is (nil? toc) "without page numbers there is nothing to link to")
    (toc/write-ncx dir toc)
    (is (cstr/includes? (slurp (io/file dir "toc.ncx")) "<text>Chapter &amp; One</text>"))))
