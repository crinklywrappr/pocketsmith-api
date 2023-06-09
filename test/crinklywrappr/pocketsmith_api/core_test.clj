(ns crinklywrappr.pocketsmith-api.core-test
  (:require [clojure.test :refer :all]
            [crinklywrappr.pocketsmith-api.core :refer :all]
            [crinklywrappr.pocketsmith-api.gen :as pgen]
            [clj-http.client :as client]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            (clj-time [core :as t]
                      [format :as f])))

(deftest parse-link-test
  (testing "parse-link works"
    (let [link (str "<https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=1>; rel=\"first\", "
                    "<https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=10>; rel=\"last\", "
                    "<https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=2>; rel=\"next\"")]
      (is (= (parse-link link)
             {:first "https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=1",
              :last "https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=10",
              :next "https://api.pocketsmith.com/v2/transaction_accounts/id/transactions?page=2"})))))

(deftest request-map-test
  (testing "request-map works"
    (is (= (request-map "abc")
           {:accept :json
            :throw-exceptions false
            :headers {:X-Developer-Key "abc"}}))))

(defspec flatten-category-test 100
  (prop/for-all [{:keys [num-elements categories]} (pgen/categories-preserve-invariants)]
                (is (== (count (mapcat flatten-category categories)) num-elements)
                    "categories should not be added or lost")
                (is (== (count (distinct (mapv :id (mapcat flatten-category categories)))) num-elements)
                    "id uniqueness should be preserved")))
