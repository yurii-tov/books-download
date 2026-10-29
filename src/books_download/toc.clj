(ns books-download.toc
  "Extracting a book's table of contents and rendering it as the EPUB
  navigation document (toc.ncx).

  The table of contents is read from the downloaded book pages themselves:
  every chapter heading is marked up on the site as

      <a name=\"gl_7\"></a><div class=\"take_h1\">Title of the chapter</div>

  where `take_h1` is a top level chapter, `take_h2` a sub-chapter and so on,
  and the `gl_N` anchor is the link target the site itself points at."
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]))


;;; --------------------------------------------------------------------------
;;; html helpers
;;; --------------------------------------------------------------------------

(def ^:private named-entities
  {"amp" "&", "lt" "<", "gt" ">", "quot" "\"", "apos" "'", "nbsp" " "
   "mdash" "\u2014", "ndash" "\u2013", "laquo" "\u00ab", "raquo" "\u00bb"
   "hellip" "\u2026", "copy" "\u00a9", "reg" "\u00ae", "deg" "\u00b0"
   "times" "\u00d7", "minus" "\u2212", "shy" ""})


(defn unescape-entities
  "Turns html entities (&amp;, &laquo;, &#171;, &#xAB;...) back into characters."
  [s]
  (cstr/replace (str s)
                #"&(#[0-9]+|#x[0-9a-fA-F]+|[a-zA-Z]+);"
                (fn [[entity code]]
                  (cond
                    (cstr/starts-with? code "#x")
                    (str (char (Integer/parseInt (subs code 2) 16)))

                    (cstr/starts-with? code "#")
                    (str (char (Integer/parseInt (subs code 1))))

                    :else (get named-entities code entity)))))


(defn strip-tags
  "Plain text of an html fragment: tags replaced by spaces (so that inline
  markup does not glue words together), entities decoded, whitespace collapsed."
  [s]
  (-> (str s)
      (cstr/replace #"(?s)<[^>]*>" " ")
      unescape-entities
      (cstr/replace #"[\u00a0\u2007\u202f]" " ")
      (cstr/replace #"\s+" " ")
      cstr/trim))


(defn escape-xml
  "Makes `s` safe to put into XML character data or an attribute value."
  [s]
  (-> (str s)
      (cstr/replace "&" "&amp;")
      (cstr/replace "<" "&lt;")
      (cstr/replace ">" "&gt;")
      (cstr/replace "\"" "&quot;")
      (cstr/replace "'" "&apos;")))


;;; --------------------------------------------------------------------------
;;; extraction
;;; --------------------------------------------------------------------------

;; The site has used two markups for chapter titles over time:
;;
;;   <a name="gl_7"></a><div class="take_h1">Title</div>      (old, depth = N)
;;   <div id="chapter-7" class="book-title">Title</div>       (current, flat)
;;
;; `book-title` is matched as a whole class token: the `book-title-line`
;; paragraphs inside a title belong to it and are not headings themselves.
(def ^:private heading-pattern
  #"(?is)<(div|p|h[1-6])\s([^>]*class\s*=\s*[\"'][^\"']*(?:title|take_h)[^\"']*[\"'][^>]*)>(.*?)</\1\s*>")

;; An anchor a chapter may start at: `<a name="gl_7">`, `id="chapter-3"`, ...
(def ^:private anchor-pattern
  #"(?i)<a\s[^>]*(?:name|id)\s*=\s*[\"']((?:gl|chapter|part)[-_]?\d+)[\"'][^>]*>")

(def ^:private id-pattern
  #"(?i)\bid\s*=\s*[\"']([^\"']+)[\"']")

(def ^:private class-pattern
  #"(?i)class\s*=\s*[\"']([^\"']*)[\"']")

;; The site's own table of contents page: <ul class="book-chapters"> ...
(def ^:private chapters-pattern
  #"(?is)<ul\s[^>]*class\s*=\s*[\"'][^\"']*book-chapters[^\"']*[\"'][^>]*>(.*?)</ul>")

(def ^:private link-pattern
  #"(?is)<a\s[^>]*href\s*=\s*[\"']([^\"']+)[\"'][^>]*>(.*?)</a>")


(defn- heading-level
  "Depth of a heading element with these classes, or nil when the element is
  not a heading (`book-title-line` and friends are parts of a title)."
  [classes]
  (let [tokens (cstr/split (cstr/trim (or classes "")) #"\s+")]
    (or (some (fn [t]
                (when-let [m (re-matches #"take_h([1-6])" t)]
                  (Integer/parseInt (second m))))
              tokens)
        (when (some #{"book-title"} tokens) 1))))


(defn- last-anchor
  "The last chapter anchor in `s` as [name end-of-match], or nil."
  [s]
  (let [matcher (re-matcher anchor-pattern s)]
    (loop [found nil]
      (if (.find matcher)
        (recur [(.group matcher 1) (.end matcher)])
        found))))


(defn- anchor-before
  "The anchor a heading at `pos` belongs to, when an anchor element stands
  directly in front of it (only whitespace and closing tags in between), e.g.
  the `gl_7` of `<a name=\"gl_7\"></a><div class=\"book-title\">Title</div>`."
  [html pos]
  (let [window (subs html (max 0 (- pos 300)) pos)]
    (when-let [[name end] (last-anchor window)]
      (when (re-matches #"(?s)(?:\s|</?[a-zA-Z][^>]*>)*" (subs window end))
        name))))


(defn html-toc
  "Table of contents entries found in the html of a single book page.

  `page` is the page file name (without extension) the entries point to, or nil
  when the html is not a downloaded page. Returns a vector of
  {:level :title :page :anchor} maps in document order."
  [html page]
  (let [html (str html)
        matcher (re-matcher heading-pattern html)]
    (loop [entries []]
      (if (.find matcher)
        (let [attributes (.group matcher 2)
              level (heading-level (second (re-find class-pattern attributes)))]
          (if-not level
            (recur entries)
            (let [title (strip-tags (.group matcher 3))]
              (recur (conj entries
                           {:level level
                            :title (if (cstr/blank? title) "-" title)
                            :anchor (or (second (re-find id-pattern attributes))
                                        (anchor-before html (.start matcher)))
                            :page page})))))
        entries))))


(defn contents-toc
  "Table of contents parsed from the site's own contents page
  (`contents.php?id=<book id>`), which lists every chapter with its title and
  the page it starts on.

  Returns nil when the page holds no table of contents, else a vector of
  {:level :title :page :anchor} maps. Page numbers are normalised to the
  downloaded page file names (\"0000000001\")."
  [html]
  (when-let [block (second (re-find chapters-pattern (str html)))]
    (let [entries (vec (for [[_ href title] (re-seq link-pattern block)
                             ;; the scraper reads the serialised DOM, where `&`
                             ;; in an attribute is escaped: id=1&amp;p=2#gl_2
                             :let [href (unescape-entities href)
                                   page (second (re-find #"[?&]p=(\d+)" href))
                                   anchor (second (re-find #"#([^\"'&\s]+)" href))
                                   title (strip-tags title)]]
                         {:level 1
                          :title (if (cstr/blank? title) "-" title)
                          :page (when page (format "%010d" (Integer/parseInt page)))
                          :anchor anchor}))]
      ;; without a page nothing can be linked to: report "no table of contents"
      ;; so that the caller scans the downloaded pages instead
      (when (some :page entries)
        entries))))


(defn- html-pages
  "Html files of a book directory, in reading order."
  [dir]
  (->> (file-seq (io/file dir))
       (filter (fn [f] (and (.isFile f)
                            (cstr/ends-with? (.getName f) ".html"))))
       (sort-by (fn [f] (.getName f)))))


(defn- page-name [f]
  (cstr/replace (.getName (io/file f)) #"\.html$" ""))


(defn file-toc
  "Table of contents of a single chapter html file."
  [f]
  (html-toc (slurp f) (page-name f)))


(defn dir-toc
  "Table of contents of a downloaded book directory, in reading order."
  [dir]
  (vec (mapcat file-toc (html-pages dir))))


(defn extract-toc
  "Extracts a book's table of contents.

  `source` is one of

    - a downloaded book directory -> every chapter of the book
    - a chapter html file         -> the chapters of that page
    - a string of html            -> the chapters found in it (no page names,
                                    so the entries carry no :page)

  Returns a vector of {:level :title :page :anchor} maps in reading order,
  where :level is the `take_hN` depth (1 = chapter, 2 = sub-chapter, ...)."
  [source]
  (cond
    (and (string? source) (cstr/starts-with? (cstr/triml source) "<"))
    (html-toc source nil)

    (instance? java.io.File source)
    (if (.isDirectory source) (dir-toc source) (file-toc source))

    (string? source) (extract-toc (io/file source))

    :else (throw (IllegalArgumentException.
                  (str "Cannot extract a table of contents from "
                       (pr-str source))))))


(defn print-toc
  "Prints the table of contents of `source` (see `extract-toc`) and returns it."
  [source]
  (let [toc (extract-toc source)]
    (doseq [{:keys [level title page anchor]} toc]
      (println (format "%s%s %s%s"
                       (apply str (repeat (dec (or level 1)) "  "))
                       (str (or level 1) ".")
                       title
                       (if page
                         (str "  ->  " page ".html" (when anchor (str "#" anchor)))
                         ""))))
    (println (format "%d chapter(s)" (count toc)))
    toc))


;;; --------------------------------------------------------------------------
;;; ncx rendering
;;; --------------------------------------------------------------------------

(defn- entry-src
  "Where a table of contents entry points at, relative to the ncx file."
  [{:keys [page anchor]}]
  (or (when page (str page ".html" (when anchor (str "#" anchor))))
      (when anchor (str "#" anchor))
      "-"))


(defn- nav-point-xml
  "Opening part of a navPoint (`</navPoint>` is written by the caller)."
  [order title src depth]
  (let [pad (apply str (repeat depth "    "))]
    (format (str "%s<navPoint id=\"navPoint-%d\" playOrder=\"%d\">\n"
                 "%s    <navLabel>\n"
                 "%s        <text>%s</text>\n"
                 "%s    </navLabel>\n"
                 "%s    <content src=\"%s\"/>\n")
            pad order order pad pad (escape-xml title) pad pad (escape-xml src))))


(declare nav-points*)

(defn- nav-points*
  "Renders the entries at `level` and deeper, nesting deeper levels as children.

  Returns [xml remaining-entries next-play-order]. `playOrder` must increase in
  document order, hence the extra bookkeeping."
  [entries level order depth]
  (loop [entries entries
         order order
         out []]
    (let [e (first entries)]
      (if (and e (>= (or (:level e) 1) level))
        (let [[children remaining order'] (nav-points* (rest entries)
                                                       (inc level)
                                                       (inc order)
                                                       (inc depth))
              pad (apply str (repeat depth "    "))]
          (recur remaining
                 order'
                 (conj out (str (nav-point-xml order (:title e) (entry-src e) depth)
                                children
                                pad "</navPoint>\n"))))
        [(apply str out) entries order]))))


(defn ncx-nav-map
  "Renders the body of the ncx <navMap> element for `toc`.

  An empty table of contents renders a single placeholder entry pointing at
  `fallback-page`, so that the ncx stays valid."
  ([toc] (ncx-nav-map toc "0000000001"))
  ([toc fallback-page]
   (let [toc (vec toc)]
     (if (seq toc)
       (first (nav-points* toc 1 1 2))
       (str (nav-point-xml 1 "\u2014" (str fallback-page ".html") 2)
            "        </navPoint>\n")))))


(defn write-ncx
  "Writes the table of contents of the book in `book-dir` into its `toc.ncx`,
  replacing the `<!-- C -->` placeholder, or the body of an existing <navMap>
  when the file carries no placeholder.

  `toc` is used when given (see `contents-toc`); otherwise the chapters are
  read from the downloaded pages. Returns the toc that was written."
  ([book-dir] (write-ncx book-dir nil))
  ([book-dir toc]
  (let [toc (if (seq toc) (vec toc) (dir-toc book-dir))
        ncx (io/file book-dir "toc.ncx")
        first-page (or (some-> (first (html-pages book-dir)) page-name)
                       "0000000001")
        points (ncx-nav-map toc first-page)
        xml (slurp ncx)
        placeholder? (cstr/includes? xml "<!-- C -->")]
    (spit ncx (if placeholder?
                ;; a regex, so that the replacement is taken literally: a
                ;; title containing `$` must not be read as a group reference
                (cstr/replace xml #"(?m)^[ \t]*<!--\s*C\s*-->[ \t]*\r?$|<!--\s*C\s*-->"
                              (fn [_] (cstr/trimr points)))
                (cstr/replace xml #"(?s)<navMap>.*</navMap>"
                              (fn [_] (str "<navMap>\n" points "    </navMap>")))))
    toc)))
