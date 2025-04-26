(defn grab-books
  "Grabs all book titles (filename-friendly) and links.
  Returns vector like [[\"name_of_a_book\" \"link-to-text.shtml\"]
                       [\"name_of_another_book\" \"text.shtml\"]
                       ...]"
  []
  (->> (find-elements (css "body > dl a"))
       (filter (fn [x] (when-let [a (element-attribute x "href")]
                         (and (re-matches #".*\.shtml" a)
                              (seq (find-elements x (css "b")))))))
       (mapv (fn [x] [(-> (find-element x (css "b"))
                          element-text
                          (cstr/replace #"[^A-Za-z0-9абвгдеёжзийклмнопрстуфхцчшщжъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЖЪЫЬЭЮЯ]" "_")
                          (cstr/replace #"__+" "_"))
                      (element-attribute x "href")]))))


(comment
  ;; example
  (doseq [[n link] books]
    (println n link)
    (open-url link)
    (spit (str "c:/Users/jurys/Downloads/chekhov/" n ".txt")
          (-> "div[align=justify]"
              css
              find-element
              element-text))))
