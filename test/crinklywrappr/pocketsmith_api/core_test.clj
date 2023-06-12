(ns crinklywrappr.pocketsmith-api.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as sg]
            [clojure.set :as st]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [crinklywrappr.pocketsmith-api.core :as ps]
            [crinklywrappr.pocketsmith-api.gen :as psgen]
            [clj-http.client :as client]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [camel-snake-kebab.core :as csk]
            (clj-time [core :as t]
                      [format :as f])))

(deftest parse-link-test
  (testing "parse-link works"
    (let [link (str "<https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=1>; rel=\"first\", "
                    "<https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=10>; rel=\"last\", "
                    "<https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=2>; rel=\"next\"")]
      (is (= (ps/parse-link link)
             {:first "https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=1",
              :last "https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=10",
              :next "https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=2"})))))

(deftest request-map-test
  (testing "request-map works"
    (is (= (ps/request-map "abc")
           {:accept :json
            :throw-exceptions false
            :headers {:X-Developer-Key "abc"}}))))

(defn get-category-ids [categories]
  ((fn get-category-ids*
     [coll [{:keys [children id] :as category} & categories]]
     (if (map? category)
       (if (seq children)
         (st/union (get-category-ids* #{} children)
                   (get-category-ids* (conj coll id) categories))
         (get-category-ids* (conj coll id) categories))
       coll)) #{} categories))

(def re-page #"[&?]page=(\d+)")

(defn get-page [token]
  (when-let [match (re-seq re-page token)]
    (Long/parseLong (second (first match)))))

(defn mock-response*
  [responses token & {:as args}]
  (let [page (or (:page args) (get-page token) 0)]
    (cond-> {:status 200
             :body (json/write-str
                    (nth responses page)
                    {:key-fn csk/->snake_case_string})}
      (and (zero? page) (> (count responses) 1))
      (assoc-in [:headers :link]
                (format "<%s?page=%d>; rel=\"next\""
                        token (inc page)))
      (< 0 page (inc page) (count responses))
      (assoc-in [:headers :link]
                (format "<%s>; rel=\"next\""
                        (sg/replace token re-page
                                    (format "?page=%d" (inc page))))))))

(defn mock-response [response-data]
  (let [responses (partition-all 10 response-data)]
    (partial mock-response* responses)))

(defn mock-error-response [responses error-page]
  (fn mock-error-response* [token & args]
    (let [page (or (get-page token) 0)]
      (if (== page error-page)
        {:status 500 :body "{\"error\" : \"an error occured\"}"}
        (mock-response* responses token :page page)))))

(deftest fetch-many-error-test
  (with-redefs [client/get (mock-error-response [[]] 0)]
    (is (= [{:status 500 :headers nil
             :body {:error "an error occured"}
             :request {:uri "uri" :key "key" :opts {}}}]
           (into [] (ps/fetch-many "uri" "key" {})))))
  (with-redefs [client/get (mock-error-response [[1 2 3] [4 5 6]] 1)]
    (is (= [1 2 3
            {:status 500 :headers nil
             :body {:error "an error occured"}
             :request {:uri "uri?page=1" :key "key" :opts {}}}]
           (into [] (ps/fetch-many "uri" "key" {}))))))

(defspec category-test 100
  (prop/for-all [{:keys [num-elements categories]} (psgen/categories-preserve-invariants)]
                (let [ids (get-category-ids categories)]
                  (with-redefs [client/get (mock-response categories)]
                    (is (== num-elements (count ids)) "sanity check #1")
                    (is (= categories (into [] (ps/categories "key" {:id 1}))) "sanity check #2")
                    (is (= (get-category-ids (into [] (ps/categories "key" {:id 1} :convert? true))) ids)
                        "convert should not add or lose categories, and id uniqueness should be preseved")
                    (is (= (get-category-ids (into [] (ps/categories "key" {:id 1} :minify? true))) ids)
                        "minify should not add or lose categories, and id uniqueness should be preseved")
                    (is (= (get-category-ids (into [] (ps/categories "key" {:id 1} :convert? true :minify? true))) ids)
                        "convert + minify should not add or lose categories, and id uniqueness should be preseved")
                    (is (= (mapv #(dissoc % :parents) (into [] (ps/categories "key" {:id 1} :flatten? true)))
                           (mapv #(dissoc % :parents) (into [] (ps/categories "key" {:id 1} :normalize? true))))
                        ":normalize? forces :flatten?, they only differ by :parents")
                    (is (= (set (mapv :id (into [] (ps/categories "key" {:id 1} :normalize? true)))) ids)
                        "normalize/flatten should not add or lose categories, and id uniqueness should be preseved")
                    (is (= (get-category-ids (into [] (ps/categories "key" {:id 1} :convert? true :minify? true :normalize? true))) ids)
                        "convert + minify + normalize should not add or lose categories, and id uniqueness should be preseved")))))
