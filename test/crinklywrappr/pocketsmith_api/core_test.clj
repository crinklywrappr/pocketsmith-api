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
            (clojurewerkz.money [amounts :as ma]
                                [currencies :as mc]
                                [format :as mf])
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
    (cond-> {:status 200}
      (< page (count responses))
      (assoc :body (json/write-str
                    (nth responses page)
                    {:key-fn csk/->snake_case_string}))
      (and (zero? page) (> (count responses) 1))
      (assoc-in [:headers :link]
                (format "<%s?page=%d>; rel=\"next\""
                        token (inc page)))
      (< 0 page (inc page) (count responses))
      (assoc-in [:headers :link]
                (format "<%s>; rel=\"next\""
                        (sg/replace token re-page
                                    (format "?page=%d" (inc page))))))))

(defn mock-response [responses]
  (partial mock-response* responses))

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

(deftest parse-datetime-conditional
  (testing "non-strings and empty string should return the value, unchanged"
    (is (= "" (ps/parse-datetime "" :date-time-no-ms)))
    (is (= :foobar (ps/parse-datetime :foobar :date-time-no-ms)))
    (is (= "" (ps/parse-local-datetime "" :date-time-no-ms)))
    (is (= :foobar (ps/parse-local-datetime :foobar :date-time-no-ms)))))

(deftest authorized-user-test
  (let [user (gen/generate psgen/user)]
    (testing "happy path"
      (with-redefs [client/get (mock-response [user])]
        (is (= user (ps/authorized-user "key")))
        (is (contains? (ps/authorized-user "key" :convert? true) :id))
        (is (contains? (ps/authorized-user "key" :minify? true) :id))
        (is (contains? (ps/authorized-user "key" :convert? true :minify? true) :id))))
    (testing "an error should make optional steps a no-op"
      (with-redefs [client/get (mock-error-response [user] 0)]
        (is (== 500 (:status (ps/authorized-user "key"))))
        (is (= (ps/authorized-user "key" :convert? true)
               (ps/authorized-user "key" :minify? true)
               (ps/authorized-user "key" :convert? true :minify? true)))))))

(defspec category-test 100
  (prop/for-all [{:keys [num-elements categories]} (psgen/categories-preserve-invariants)]
                (let [ids (get-category-ids categories)]
                  (with-redefs [client/get (mock-response (partition-all 10 categories))]
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

(deftest category-problem-test
  (testing "zero categories"
    (with-redefs [client/get (mock-response [])]
      (is (= [{:status 200
               :body nil
               :parse-error "class java.lang.NullPointerException"
               :headers nil
               :request {:uri "https://api.pocketsmith.com/v2/users/1/categories"
                         :key "key" :opts {}}}]
             (into [] (ps/categories "key" {:id 1}))
             (into [] (ps/categories "key" {:id 1} :convert? true :minify? true :normalize? true))))))
  (testing "problem fetching categories"
    (let [categories (partition-all 3 (gen/generate (psgen/categories :min-elements 50 :max-depth 5)))]
      (with-redefs [client/get (mock-error-response categories 0)]
        (is (== 1 (count (into [] (ps/categories "key" {:id 1})))))
        (is (ps/error-response? (first (into [] (ps/categories "key" {:id 1})))))
        (is (= (into [] (ps/categories "key" {:id 1}))
               (into [] (ps/categories "key" {:id 1} :convert? true :minify? true :normalize? true)))))
      (with-redefs [client/get (mock-error-response categories 1)]
        (is (> (count (into [] (ps/categories "key" {:id 1}))) 1))
        (is (ps/error-response? (last (into [] (ps/categories "key" {:id 1})))))
        (is (= (last (into [] (ps/categories "key" {:id 1})))
               (last (into [] (ps/categories "key" {:id 1} :convert? true :minify? true :normalize? true)))))
        (is (every? ps/category? (butlast (into [] (ps/categories "key" {:id 1})))))
        (is (every? ps/category? (butlast (into [] (ps/categories "key" {:id 1} :convert? true :minify? true :normalize? true)))))))))

(defspec money-test 10
  (prop/for-all [monies (apply gen/tuple (mapv #(psgen/money* % {}) (mc/registered-currencies)))]
                (is (= (vec monies)
                       (mapv
                        (fn [money]
                          (let [code (sg/lower-case (ma/currency-of money))
                                amount (psgen/money->bigdec money)]
                            (ps/amount->money amount code)))
                        monies)))))

(defspec usd-test 100
  (prop/for-all [money (psgen/money* mc/USD {})]
                (let [code (sg/lower-case (ma/currency-of money))
                      amount (psgen/money->bigdec money)]
                  (is (= money (ps/amount->money amount code))))))

(defspec jpy-test 100
  (prop/for-all [money (psgen/money* mc/JPY {})]
                (let [code (sg/lower-case (ma/currency-of money))
                      amount (psgen/money->bigdec money)]
                  (is (= money (ps/amount->money amount code))))))

(deftest amount->money-conditional-test
  (is (= "" (ps/amount->money "" "")))
  (is (= (bigdec 0) (ps/amount->money (bigdec 0) nil)))
  (is (= (bigdec 0) (ps/amount->money (bigdec 0) ""))))

(defspec account-test 50
  (prop/for-all [user psgen/user]
                (let [accounts (gen/generate (gen/vector (psgen/transaction-account user) 1 24))]
                  (with-redefs [client/get (mock-response (partition-all 10 accounts))]
                    (is (= accounts (into [] (ps/accounts "key" user))))
                    (is (= (mapv :id accounts) (mapv :id (into [] (ps/accounts "key" user :convert? true)))))
                    (is (= (mapv :id accounts) (mapv :id (into [] (ps/accounts "key" user :minify? true)))))))))

(deftest by-name-or-title-test
  (let [xs (mapv (fn [n] {:name (str n)}) (range 20))]
    (is (= {:name "10"} (ps/by-name-or-title "10" xs))))
  (let [xs (mapv (fn [n] {:title (str n)}) (range 20))]
    (is (= {:title "10"} (ps/by-name-or-title "10" xs)))))

(deftest account-problem-test
  (testing "zero accounts"
    (with-redefs [client/get (mock-response [])]
      (is (= [{:status 200
               :body nil
               :parse-error "class java.lang.NullPointerException"
               :headers nil
               :request {:uri "https://api.pocketsmith.com/v2/users/1/transaction_accounts"
                         :key "key" :opts {}}}]
             (into [] (ps/accounts "key" {:id 1}))
             (into [] (ps/accounts "key" {:id 1} :convert? true :minify? true))))))
  (testing "problem fetching accounts"
    (let [user (gen/generate psgen/user)
          accounts (partition-all 3 (gen/generate (gen/vector (psgen/transaction-account user) 9)))]
      (with-redefs [client/get (mock-error-response accounts 0)]
        (is (== 1 (count (into [] (ps/accounts "key" user)))))
        (is (ps/error-response? (first (into [] (ps/accounts "key" user)))))
        (is (= (into [] (ps/accounts "key" user))
               (into [] (ps/accounts "key" user :convert? true :minify? true)))))
      (with-redefs [client/get (mock-error-response accounts 1)]
        (is (> (count (into [] (ps/accounts "key" user))) 1))
        (is (ps/error-response? (last (into [] (ps/accounts "key" user)))))
        (is (= (last (into [] (ps/accounts "key" user)))
               (last (into [] (ps/accounts "key" user :convert? true :minify? true)))))
        (is (every? #(contains? % :id) (butlast (into [] (ps/accounts "key" user)))))
        (is (every? #(contains? % :id) (butlast (into [] (ps/accounts "key" user :convert? true :minify? true)))))))))
