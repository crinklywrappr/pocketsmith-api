(ns crinklywrappr.pocketsmith-api.core-test
  (:require [clojure.test :refer :all]
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

(defspec category-test 100
  (prop/for-all [{:keys [num-elements categories]} (psgen/categories-preserve-invariants)]
                (let [ids (get-category-ids categories)]
                  (with-redefs [client/get
                                (fn [& args]
                                  {:status 200
                                   :body (json/write-str
                                          categories
                                          {:key-fn csk/->snake_case_string})})]
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
