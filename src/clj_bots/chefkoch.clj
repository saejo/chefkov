(ns clj-bots.chefkoch
  (:require [clojure.string :as str]
            [clojure.java.jdbc :refer :all]
            [net.cgrand.enlive-html :as html]))

(def test-url "https://www.chefkoch.de/rezepte/2653511416758959/Pikanter-Dattel-Frischkaese-Dip.html")

(def greetings #{"LG" "Liebe Grüße" "Gruß" "Lieben Gruß" "MfG" "MFG" "mfg" "lg" "Lg" "vg" "glg" "GLG" "VG" "Viele Grüße" "Grüße"})

(defn starts-with-greeting? [s]
  (some #{true}
        (map #(str/starts-with? s %)
             greetings)))

;; TODO: env vars
(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     #_"/home/jon/src/clj-bots/database.db "
   "/home/pi/bots/chefkov/database.db"})

(defn fetch-page [url]
  (html/html-resource (java.net.URL. url)))

(defn as [url]
  (html/select (fetch-page url) [:a.product-tile]))

(defn remove-whitespace [s]
  (str/join " "
                       (filter #(not (str/blank? %))
                               (str/split s #" "))))

(defn page->ingredients [p]
  (let [rows (html/select p [:table.ingredients :tr])
        lefts (map :content (html/select rows [:td.td-left :span]))
        right-spans (html/select rows [:td.td-right :span])]
    (map (fn [s]
           (remove-whitespace s))
         (map (fn [s]
                (clojure.string/replace s "\n" ""))
              (map #(apply  str %) (partition 2 (interleave (map first  lefts)
                                                            (map (fn [rs] (if (map? (first (:content rs)))
                                                                            (first (:content (first (:content rs))))
                                                                            (first (:content rs)))) right-spans))))))))

(defn page->avg-rating [p]
  (java.lang.Float/parseFloat (first (:content (first (html/select p [:div.ds-rating-avg :span :strong]))))))

(defn page->rating-count [p]
  (java.lang.Integer/parseInt (first (:content (nth (:content (first (html/select p [:div.ds-rating-count :span]))) 2)))))

(defn page->comment-count [p]
  (try
    (let [count(-> (html/select p [:div.recipe-meta-btns :button.recipe-comments-anchor])
                   first
                   :content
                   (nth 3)
                   :content
                   first
                   :content
                   first
                   (java.lang.Integer/parseInt))]
      count)
    (catch Exception e
      0)))

(defn page->title [p]
  (first (:content (first (html/select p [:title])))))

(defn page->user [p]
  (first (:content (second (html/select  p [:a.bi-profile :span])))))

(defn page->tags [p]
  (->> (html/select p [:a.ds-tag])
       (map #(-> % :content first (str/replace #" " "") (str/replace #"\n" "")) )
       (str/join " " )))

(defn fetch-comments [n]
  (try
    (loop [i 0
           comments []
           progress (apply str (repeat n "-"))]
      (prn progress)
      (if (= i n)
        comments
        (let [p (fetch-page "https://www.chefkoch.de/rezepte/zufallsrezept/")
              contents (map :content (html/select p [:div.qa-comment :div.ds-mb-right :p]))
              contents (remove #(or (= % '("Dein Kommentar wurde erfolgreich gespeichert."))
                                    (= % '("Dein Kommentar wird gespeichert..."))
                                    (= % '("Dein Kommentar konnte nicht gespeichert werden."))) contents)
              contents (map first contents)
              contents (map #(str/replace % "\n" " ") contents)
              contents (map #(str/replace % "  " " ") contents)]
          (recur (inc i)
                 (concat comments contents)
                 (str/replace-first progress "-" "X")))))
    (catch Exception e
      (prn e))))

(defn clean-up-comment [c]
  (some-> c
          (str/replace "\n" " ")
          (str/replace "  " " ")))

(defn reformat-date [d]
  (let [year (apply str (take 4 (drop 6 d)))
        month (apply str (take 2 (drop 3 d)))
        day (apply str (take 2 d ))]
    (str/join "-" [year month day])))

(defn comment-block->comment-with-date [b]
  (let [text (-> b
                 first
                 :content
                 first
                 clean-up-comment)
        date (-> b
                 (nth 2)
                 :content
                 second
                 :content
                 first
                 reformat-date)]
    {:text text
     :date date}))

#_(query db "select text from comments where date BETWEEN date('2005-01-01') AND date('2010-12-31')")

(defn page->comments-with-date [p]
  (let [comment-block-right (html/select p [:div.qa-comment :div.ds-mb-right])
        blocks (html/select comment-block-right {[:p] [:div]})]
    (map comment-block->comment-with-date blocks)))

(defn page->instructions [p]
  (let [boxes (html/select p [:article.ds-box])
        the-right-box (filter #(= "Zubereitung" (first (:content (first (html/select % [:h2]))))) boxes)
        the-inner-box (html/select the-right-box [:div.ds-box])]
    (clojure.string/replace
     (->> the-inner-box
          first
          :content
          (filter string?)
          (apply str))
     "\n" "")))

(defn page->prep-time [p]
  (java.lang.Integer/parseInt
   (first (str/split (str/trim (last(:content (first (html/select p [:span.recipe-preptime]))))) #"\s"))))

(defn page->difficulty [p]
  (str/trim (last(:content (first (html/select p [:span.recipe-difficulty]))))))

(defn page->date [p]
  (str/trim (last(:content (first (html/select p [:span.recipe-date]))))))

(defn fetch-comments-eifel  [db n]
  (prn "Fetching comments from " n " pages")
  (with-db-connection [db db]
    (doseq [f (range n)]
      (try
        (println (inc f) "/" n)
        (let [p (fetch-page "https://www.chefkoch.de/rezepte/zufallsrezept/")
              difficulty (page->difficulty p)
              duration (page->prep-time p)
              date (page->date p)
              user (page->user p)
              o-url (:content (first(filter #(= (:property %) "og:url") (map :attrs (html/select p [:meta])))))
              ingredients (page->ingredients p)
              title (first (:content (first (html/select p [:title]))))
              comments (page->comments-with-date p)
              instructions (page->instructions p)
              rating-count (page->rating-count p)
              rating-avg (page->avg-rating p)
              comment-count (page->comment-count p)
              tags (page->tags p)]
          (insert! db :pages {:title title
                              :rating_avg rating-avg
                              :rating_count rating-count
                              :comment_count comment-count
                              :html p
                              :url o-url
                              :duration duration
                              :difficulty difficulty
                              :date date
                              :user user
                              :tags tags})
          (insert! db :instructions {:instruction instructions
                                     :url o-url})

          ;; TODO: ingredients seperat, fetchen und storen trennen
          ;; ODer?
          (doseq [ii ingredients]
            (insert! db :ingredients {:ingredients ii
                                      :url o-url}))

          (doseq [c comments]
            (let [id (java.util.UUID/randomUUID)]
              (insert! db :comments {:text (:text c)
                                     :date (:date c)
                                     :page title
                                     :id id
                                     :url o-url}))))
        (catch Exception e
          (prn e))))))

(defn store-comments [db n]
  (let [comments (->> (fetch-comments n)
                      (remove #(re-find #"\d" %)))]
    (doseq [c comments]
      (insert! db :comments {:text c}))))

(defn fetch-db-comments [db]
  (query db ["select text from comments"] {:row-fn :text}))

(defn fetch-generated-comments [db]
  (query db ["select text from generated_comments"] {:row-fn :text}))

(defn fetch-generated-comments-all [db]
  (query db ["select * from generated_comments"] ))


(defn comments-with-sub-string [db substring]
  (query db ["select text from generated_comments where instr(lower(text),lower(?)) != 0" substring] ))

(defn original-comments-with-sub-string [db substring]
  (query db ["select text from comments where instr(lower(text),lower(?)) != 0" substring] ))

(defn word-counts- [db]
  (frequencies (str/split (str/lower-case (str/join " " (fetch-db-comments db))) #"\s")))

(defn word-counts-generated- [db]
  (frequencies (str/split (str/join " " (fetch-generated-comments db)) #"\s")))

(def word-counts-generated (memoize word-counts-generated-))

(def word-counts (memoize word-counts-))

(defn lower-frequency [db a b]
  (let [wcs (word-counts db)]
    (cond (< (get wcs a)
             (get wcs b)) a
          (< (get wcs b)
             (get wcs a)) b
          (> (count a) (count b)) a
          :else b)))

;; TODO nil behanden
(defn lower-frequency-generated [db a b]
  (let [wcs (word-counts-generated db)]
    (cond (< (get wcs a)
             (get wcs b)) a
          (< (get wcs b)
             (get wcs a)) b
          (> (count a) (count b)) a
          :else b)))

(defn get-random-db-comment
  ([db ]
   (get-random-db-comment db {}))
  ([db opts]
   (with-db-connection [db db]
     (let [original-comment-count (if (:new opts)
                                    (first (vals (query db ["select count(*) from comments"] {:result-set-fn first
                                                                                              :query-fn :count})))
                                    0)
           query-str (str
                      "select * from generated_comments where used = 'false' "
                      (when (:new opts)
                        "and original_comments_count = ? ")
                      "order by random() limit 1")]
       (prn query-str)
       (prn original-comment-count)
       (query db (if (:new opts)
                   [query-str original-comment-count]
                   [query-str])
              {:result-set-fn first})))))

(defn get-answer [db text]
  (let [wcs (word-counts-generated db)
        text (str/lower-case text)
        text (str/replace text "?" "")
        text (str/replace text "!" "")
        text (str/replace text "." "")
        text (str/replace text "," "")
        words (str/split text #"\s")
        freqs (reduce (fn [a b]
                        (assoc a b (get wcs b)))
                      {}
                      words)
        ordered (sort-by second freqs)
        ordered (filter #(-> % second some?) ordered)]
    (prn "ordered: " ordered)
    (loop [ordered ordered]
      (if (nil? (second (last ordered)))
        (do
          (prn "random")
          (-> (get-random-db-comment db) :text))
        (let [response     (->> ordered
                                first
                                first
                                (comments-with-sub-string db))]
          (if (pos? (count response))
            (do
              (prn (first (first ordered)))
              (-> response first :text))
            (recur (rest ordered))))))))

(defn comments-with-substring [db substring]
  (original-comments-with-sub-string db substring))

(defn get-random-db-comments [db limit]
  (query db ["select text from generated_comments order by random() limit ?" limit]))

(defn mark-comment-as-used [db id]
    (execute! db ["update generated_comments set used = 'true' where uid = ?" id]))

(defn greeting-in-middle? [comment]
  ;; todo: make sure only one greeting
  (let [strings (str/split comment #" ")
        strings (->> strings
                     (map #(str/replace % #"," ""))
                     (map #(str/replace % #"!" ""))
                     (map #(str/replace % #"\." ""))
                     (map #(str/replace % #"\?" "")))
        length (count strings)
        ;; Komma und so am Ende der Strings weg
        contained-greeting (some greetings strings)
        nr-of-greetings (count (filter #(some greetings [%]) strings))]
    (or (not (<= nr-of-greetings 1))
     (and contained-greeting
          ;; the grreting should be next to last
          (not= contained-greeting
                (nth strings (- length 2)))))))

(defn hallo-in-middle? [comment]
  (try
    (let [strings (str/split comment #" ")
          strings (->> strings
                       (map #(str/replace % #"," ""))
                       (map #(str/replace % #"!" ""))
                       (map #(str/replace % #"\." ""))
                       (map #(str/replace % #"\?" "")))
          hallo? (some #(str/starts-with? % "hallo" ) (map str/lower-case strings))
          hi? (or (some #(str/starts-with? % "hi" ) (map str/lower-case strings)))
          length (count strings)]
      (and (or
            hallo?
            hi?)
           (not= (str/lower-case (nth strings 0)) "hallo")))
    (catch Exception e
      (prn "Exception bei : " comment))))
