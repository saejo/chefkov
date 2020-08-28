(ns clj-bots.twitter
  (:require [clj-bots.chefkoch :as chefkoch]
            [clojure.string :as str]
            [twitter.oauth :as oauth]
            [twitter.api.restful :as restful]
            [clojure.java.jdbc :refer :all]
            [http.async.client :as ac])

  (:gen-class))

(def app-consumer-key "")
(def app-consumer-secret "")
(def user-access-token "")
(def user-access-token-secret "")

(def chefkov-creds
  (oauth/make-oauth-creds app-consumer-key
                          app-consumer-secret
                          user-access-token
                          user-access-token-secret))

(defn tweet [text]
  (prn "tweeting: " text)
  (with-open [client (http.async.client/create-client)]
    (restful/statuses-update :oauth-creds chefkov-creds
                             :client client
                             :params {:status text})))

(defn tweet-random-comment []
  (prn "Tweeting random comment")
  (let [{:keys [text uid]} (chefkoch/get-random-db-comment chefkoch/db {:new true})]
    (if (nil? text)
      (let [{:keys [text uid]} (chefkoch/get-random-db-comment chefkoch/db {:new false})]
        (try
          (tweet text)
          (chefkoch/mark-comment-as-used chefkoch/db uid)
          (catch Exception e
            (prn e))))
      (try
        (tweet text)
        (chefkoch/mark-comment-as-used chefkoch/db uid)
        (catch Exception e
          (prn e))))))

(def kochi-user-id 1104448709205602311)

(defn fetch-mentions []
  (with-open [client (http.async.client/create-client)]
    (let [req (restful/statuses-mentions-timeline :client client :oauth-creds chefkov-creds)]
      (-> req :body))))

(defn store-mention [mention]
  (let [text (str/join " " (remove #(str/starts-with? % "@")
                                   (-> mention :text (str/split #"\s") )))]
    (insert! chefkoch/db :mentions {:text text
                                    :original_text (:text mention)
                                    :reply_to (:in_reply_to_status_id mention)
                                    :id (:id mention)
                                    :user (-> mention :user :screen_name)
                                    :user_id (-> mention :user :id)
                                    ;; TODO einmal korrekte werte rein, dann hier keinen mehr setzen?
                                    :replied_to false})))

(defn store-mentions []
  (prn "storing mentions")
  (let [mentions (fetch-mentions)
        stored-mentions (query chefkoch/db ["select id from mentions"] {:row-fn :id})]
    (doseq [m mentions]
      (when-not (some #{(:id m)} stored-mentions)
        (prn "storing: " m)
        (store-mention m)))))

(defn unanswered-mentions []
  (let [mentions (query chefkoch/db "select * from mentions")]
    (filter #(= 0 (:replied_to %)) mentions)))

;; TODO: ids speichern
;; TODO: speichern, auf wen geantwortet wurde
(defn fetch-tweets []
  (with-open [client (http.async.client/create-client)]
    (-> (restful/statuses-user-timeline
         :client client
         :oauth-creds chefkov-creds)
        :body)))

(defn mark-mention-as-answered [id]
  (execute! chefkoch/db ["update mentions set replied_to = 1 where id = ?" id]))

(defn reply-to-tweet [tweet-id user-name reply-text]
  (let [complete-text (str "@" user-name " " reply-text)]
    (prn complete-text)
    (with-open [client (http.async.client/create-client)]
      (restful/statuses-update :oauth-creds chefkov-creds
                               :client client
                               :params {:status complete-text
                                        :in_reply_to_status_id tweet-id}))


    (let [mark (mark-mention-as-answered tweet-id)]
       (prn "mark " mark))

    (let [delete (execute! chefkoch/db ["delete from generated_comments where text = ? " reply-text])]
        (prn "delete: " delete))))

(defn reply-to-mentions [kochi?]
  (prn "Replying to mentions. With kochi? " kochi?)
  (let [mentions (if kochi?
                   (unanswered-mentions)
                   (remove #(= (:user_id %) kochi-user-id) (unanswered-mentions)))]
    (prn mentions)
    (prn "found " (count mentions) " unanswered mentions")
    (doseq [m mentions]
      (let [text (:text m)
            #_#_reply (:text (chefkoch/get-random-db-comment chefkoch/db))
            reply (chefkoch/get-answer chefkoch/db text)]
        (reply-to-tweet (:id m) (:user m) reply)))))

(defn reply-to-random-chefkoch-mention []
  (prn "Replying to random tweet")
  (with-open [client (http.async.client/create-client)]
    (let [status (->> (:statuses (:body (restful/search-tweets :oauth-creds chefkov-creds
                                                               :client client
                                                               :params {:q "chefkoch.de"})))
                    (sort-by :favorite_count)
                    last)
        id (:id status)
        user (:user status)
        urls (:urls (:entities  status))
        url (:expanded_url (filter #(str/starts-with? (:expanded_url %) "https://www.chefkoch.de")
                                   (:urls (:entities  status))))]
      (if url
        (let [p (chefkoch/fetch-page url)
              title (str/split (chefkoch/page->title p) #"von")
              title (remove #(= "-" %)
                            (remove #(= "|" %)
                                    (remove #(= "Chefkoch" %)
                                            title)))
              title (first (sort-by count title))
              reply-text (:text (chefkoch/get-answer chefkoch/db title))
              #_#_reply-text (:text (chefkoch/get-random-db-comment chefkoch/db))]
          (reply-to-tweet id (:screen_name (:user status)) reply-text))
        (let [text (:text status)
              #_#_reply-text (:text (chefkoch/get-answer chefkoch/db text))
              reply-text (:text (chefkoch/get-random-db-comment chefkoch/db))]
          (reply-to-tweet id (:screen_name (:user status) )reply-text))))))

(defn -main [& args]
  (store-mentions)
  (when (some #{"reply"} args)
    (reply-to-mentions false))
  (when (some #{"kochi"} args)
    (reply-to-mentions true))
  (when (some #{"tweet"} args)
    (tweet-random-comment))
  (when (some #{"fetch"} args)
    (chefkoch/fetch-comments-eifel chefkoch/db 100))
  (when (some #{"random"} args)
    (reply-to-random-chefkoch-mention)))
