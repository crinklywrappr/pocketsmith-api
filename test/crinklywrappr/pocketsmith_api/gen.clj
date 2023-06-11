(ns crinklywrappr.pocketsmith-api.gen
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            (clj-time [core :as t]
                      [format :as f])))

(defn rrest [xs] (rest (rest xs)))
(defn index-map [xs] (into {} (map-indexed vector xs)))

(defn order
  ([g] (gen/fmap #(sort %) g))
  ([cmp g] (gen/fmap #(sort cmp %) g)))

(defn order-by
  ([keyfn g] (gen/fmap #(sort-by keyfn < %) g))
  ([keyfn cmp g] (gen/fmap #(sort-by keyfn cmp < %) g)))

(defn order-by [g keyfn & {:keys [cmp] :or {}}])

(defn- nest-fold*
  [nestf [children parents :as a] pk]
  (if (seq children)
    (reduce
     (fn [[children parents :as a] _]
       (let [[ck child] (rand-nth (seq children))]
         [(dissoc children ck) (update parents pk nestf child)]))
     a (range (inc (rand-int (dec (count children))))))
    a))

(defn- nest-fold [nestf groups]
  ;; free to shuffle here since all remaining
  ;; groups will have identical lengths.
  ;; it makes the output look more random.
  (shuffle
   (concat
    [(second
      (reduce (partial nest-fold* nestf)
              (take 2 groups)
              (keys (second groups))))]
    (rrest groups))))

(defn nest* [nestf max-depth xs]
  (let [group-sz (long (/ (count xs) max-depth))
        ;; why reverse?  because the last partition
        ;; may be shorter and we need it at the front
        groups (reverse (partition-all group-sz xs))]
    (->
     (iteration
      (partial nest-fold nestf)
      :somef #(>= (count %) 1)
      :kf #(when (> (count %) 1) %)
      ;; why maps?  because `dissoc` is difficult
      ;; and costly to emulate on other structures
      :initk (map index-map groups))
     last first vals)))

(defn nest
  [nestf g & {:keys [max-depth] :or {max-depth 2}}]
  (gen/fmap (partial nest* nestf max-depth) g))

;; the variant components are handled differently
;; than t/date-time: instead of being zeroed out,
;; they are randomized.
(defn date-time
  ([]
   (gen/let [year (gen/choose 1970 (t/year (t/now)))
             month (gen/choose 1 12)
             day (gen/choose 1 28)
             hour (gen/choose 0 23)
             minute (gen/choose 0 59)
             second (gen/choose 0 59)
             millis (gen/choose 0 999)]
     (date-time year month day hour minute second millis)))
  ([year]
   (gen/let [year (gen/return year)
             month (gen/choose 1 12)
             day (gen/choose 1 28)
             hour (gen/choose 0 23)
             minute (gen/choose 0 59)
             second (gen/choose 0 59)
             millis (gen/choose 0 999)]
     (date-time year month day hour minute second millis)))
  ([year month]
   (gen/let [year (gen/return year)
             month (gen/return month)
             day (gen/choose 1 28)
             hour (gen/choose 0 23)
             minute (gen/choose 0 59)
             second (gen/choose 0 59)
             millis (gen/choose 0 999)]
     (date-time year month day hour minute second millis)))
  ([year month day]
   (gen/let [year (gen/return year)
             month (gen/return month)
             day (gen/return day)
             hour (gen/choose 0 23)
             minute (gen/choose 0 59)
             second (gen/choose 0 59)
             millis (gen/choose 0 999)]
     (date-time year month day hour minute second millis)))
  ([year month day hour]
   (gen/let [year (gen/return year)
             month (gen/return month)
             day (gen/return day)
             hour (gen/return hour)
             minute (gen/choose 0 59)
             second (gen/choose 0 59)
             millis (gen/choose 0 999)]
     (date-time year month day hour minute second millis)))
  ([year month day hour minute]
   (gen/let [year (gen/return year)
             month (gen/return month)
             day (gen/return day)
             hour (gen/return hour)
             minute (gen/return minute)
             second (gen/choose 0 59)
             millis (gen/choose 0 999)]
     (date-time year month day hour minute second millis)))
  ([year month day hour minute second]
   (gen/let [year (gen/return year)
             month (gen/return month)
             day (gen/return day)
             hour (gen/return hour)
             minute (gen/return minute)
             second (gen/return second)
             millis (gen/choose 0 999)]
     (date-time year month day hour minute second millis)))
  ([year month day hour minute second millis]
   (gen/return (t/date-time year month day hour minute second millis))))

(defn string-date
  ([fmt]
   (gen/fmap #(f/unparse (f/formatter fmt) %) (date-time)))
  ([dt fmt]
   (gen/fmap #(f/unparse (f/formatter fmt) %) (gen/return dt))))

(def category
  (gen/let [[t1 t2] (order t/before? (gen/tuple (date-time) (date-time)))]
    (gen/hash-map
     :id (gen/large-integer* {:min 0})
     :title gen/string-ascii

     :created-at (string-date t1 :date-time-no-ms)
     :updated-at (string-date t2 :date-time-no-ms)

     :parent-id (gen/return nil)
     :children (gen/return [])

     :roll-up gen/boolean
     :is-bill gen/boolean
     :is-transfer gen/boolean

     ;;refund-behaviour and colour are incorrect
     :refund-behaviour gen/string-ascii
     :colour gen/string-ascii)))

(defn categories
  [& {:keys [max-depth min-elements max-elements] :as opts}]
  (nest
   (fn [{:keys [id] :as parent} child]
     (update parent :children conj (assoc child :parent-id id)))
   (gen/not-empty
    (gen/vector-distinct-by
     :id category {:min-elements min-elements :max-elements max-elements}))
   opts))

(defn categories-preserve-invariants
  ([]
   (gen/let [num-elements (gen/large-integer* {:min 20 :max 100})
             max-depth (gen/large-integer* {:min 2 :max 10})]
     (categories-preserve-invariants num-elements max-depth)))
  ([num-elements max-depth]
   (gen/fmap
    (fn [categories]
      {:num-elements num-elements
       :max-depth max-depth
       :categories categories})
    (categories :min-elements num-elements
                :max-elements num-elements
                :max-depth max-depth))))

(defn count-categories [categories]
  ((fn count-categories*
     [c [{:keys [children] :as category} & categories]]
     (if (map? category)
       (if (seq children)
         (+ (count-categories* 0 children)
            (count-categories* (inc c) categories))
         (count-categories* (inc c) categories))
       c)) 0 categories))

(defn find-max-depth [categories]
  ((fn find-max-depth*
     [c [{:keys [children] :as category} & categories]]
     (if (map? category)
       (if (seq children)
         (max (find-max-depth* (inc c) children)
              (find-max-depth* c categories))
         (find-max-depth* c categories))
       c)) 0 categories))

(defspec category-dates-make-sense 100
  (prop/for-all [{:keys [created-at updated-at]} category]
                (let [created-at (f/parse (f/formatter :date-time-no-ms) created-at)
                      updated-at (f/parse (f/formatter :date-time-no-ms) updated-at)]
                  (is (or (t/before? created-at updated-at)
                          (t/equal? created-at updated-at))))))

(defspec categories-preserves-count 100
  (prop/for-all [{:keys [num-elements max-depth categories]} (categories-preserve-invariants)]
                (is (== (count-categories categories) num-elements))
                (is (< (find-max-depth categories) num-elements))))