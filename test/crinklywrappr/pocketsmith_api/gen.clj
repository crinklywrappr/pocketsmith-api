(ns crinklywrappr.pocketsmith-api.gen
  (:require [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as sg]
            (clojurewerkz.money [amounts :as ma]
                                [currencies :as mc]
                                [format :as mf])
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

(defn country-codes [currency] (.getCountryCodes currency))

(def currency
  (gen/one-of (mapv gen/return (mc/registered-currencies))))

(def currency-with-country-code
  (->> (mc/registered-currencies)
       (filter (comp seq country-codes))
       (mapv gen/return) gen/one-of))

(defn money*
  ([{:keys [min max] :or {min (Long/MIN_VALUE) max (Long/MAX_VALUE)} :as opts}]
   (gen/bind currency #(money* % opts)))
  ([currency {:keys [min max] :or {min (Long/MIN_VALUE) max (Long/MAX_VALUE)} :as opts}]
   (gen/fmap (partial ma/of-minor currency) (gen/large-integer* opts))))

(def money (money* {}))

(defn money->bigdec [amt]
  (-> amt str (sg/split #" ") second bigdec))

(def time-zones
  [{:name "International Date Line West",
    :utc-offset -39600,
    :formatted-name "(GMT-11:00) International Date Line West",
    :formatted-offset "-11:00",
    :abbreviation "SST",
    :identifier "Pacific/Midway"}
   {:name "Midway Island",
    :utc-offset -39600,
    :formatted-name "(GMT-11:00) Midway Island",
    :formatted-offset "-11:00",
    :abbreviation "SST",
    :identifier "Pacific/Midway"}
   {:name "Samoa",
    :utc-offset -39600,
    :formatted-name "(GMT-11:00) Samoa",
    :formatted-offset "-11:00",
    :abbreviation "SST",
    :identifier "Pacific/Pago_Pago"}
   {:name "Hawaii",
    :utc-offset -36000,
    :formatted-name "(GMT-10:00) Hawaii",
    :formatted-offset "-10:00",
    :abbreviation "HST",
    :identifier "Pacific/Honolulu"}
   {:name "Alaska",
    :utc-offset -28800,
    :formatted-name "(GMT-08:00) Alaska",
    :formatted-offset "-08:00",
    :abbreviation "AKDT",
    :identifier "America/Juneau"}
   {:name "Pacific Time (US & Canada)",
    :utc-offset -25200,
    :formatted-name "(GMT-07:00) Pacific Time (US & Canada)",
    :formatted-offset "-07:00",
    :abbreviation "PDT",
    :identifier "America/Los_Angeles"}
   {:name "Arizona",
    :utc-offset -25200,
    :formatted-name "(GMT-07:00) Arizona",
    :formatted-offset "-07:00",
    :abbreviation "MST",
    :identifier "America/Phoenix"}
   {:name "Tijuana",
    :utc-offset -25200,
    :formatted-name "(GMT-07:00) Tijuana",
    :formatted-offset "-07:00",
    :abbreviation "PDT",
    :identifier "America/Tijuana"}
   {:name "Chihuahua",
    :utc-offset -21600,
    :formatted-name "(GMT-06:00) Chihuahua",
    :formatted-offset "-06:00",
    :abbreviation "MDT",
    :identifier "America/Chihuahua"}
   {:name "Mountain Time (US & Canada)",
    :utc-offset -21600,
    :formatted-name "(GMT-06:00) Mountain Time (US & Canada)",
    :formatted-offset "-06:00",
    :abbreviation "MDT",
    :identifier "America/Denver"}
   {:name "Central America",
    :utc-offset -21600,
    :formatted-name "(GMT-06:00) Central America",
    :formatted-offset "-06:00",
    :abbreviation "CST",
    :identifier "America/Guatemala"}
   {:name "Mazatlan",
    :utc-offset -21600,
    :formatted-name "(GMT-06:00) Mazatlan",
    :formatted-offset "-06:00",
    :abbreviation "MDT",
    :identifier "America/Mazatlan"}
   {:name "Saskatchewan",
    :utc-offset -21600,
    :formatted-name "(GMT-06:00) Saskatchewan",
    :formatted-offset "-06:00",
    :abbreviation "CST",
    :identifier "America/Regina"}
   {:name "Bogota",
    :utc-offset -18000,
    :formatted-name "(GMT-05:00) Bogota",
    :formatted-offset "-05:00",
    :abbreviation "-05",
    :identifier "America/Bogota"}
   {:name "Central Time (US & Canada)",
    :utc-offset -18000,
    :formatted-name "(GMT-05:00) Central Time (US & Canada)",
    :formatted-offset "-05:00",
    :abbreviation "CDT",
    :identifier "America/Chicago"}
   {:name "Lima",
    :utc-offset -18000,
    :formatted-name "(GMT-05:00) Lima",
    :formatted-offset "-05:00",
    :abbreviation "-05",
    :identifier "America/Lima"}
   {:name "Quito",
    :utc-offset -18000,
    :formatted-name "(GMT-05:00) Quito",
    :formatted-offset "-05:00",
    :abbreviation "-05",
    :identifier "America/Lima"}
   {:name "Guadalajara",
    :utc-offset -18000,
    :formatted-name "(GMT-05:00) Guadalajara",
    :formatted-offset "-05:00",
    :abbreviation "CDT",
    :identifier "America/Mexico_City"}
   {:name "Mexico City",
    :utc-offset -18000,
    :formatted-name "(GMT-05:00) Mexico City",
    :formatted-offset "-05:00",
    :abbreviation "CDT",
    :identifier "America/Mexico_City"}
   {:name "Monterrey",
    :utc-offset -18000,
    :formatted-name "(GMT-05:00) Monterrey",
    :formatted-offset "-05:00",
    :abbreviation "CDT",
    :identifier "America/Monterrey"}
   {:name "Caracas",
    :utc-offset -14400,
    :formatted-name "(GMT-04:00) Caracas",
    :formatted-offset "-04:00",
    :abbreviation "-04",
    :identifier "America/Caracas"}
   {:name "Indiana (East)",
    :utc-offset -14400,
    :formatted-name "(GMT-04:00) Indiana (East)",
    :formatted-offset "-04:00",
    :abbreviation "EDT",
    :identifier "America/Indiana/Indianapolis"}
   {:name "La Paz",
    :utc-offset -14400,
    :formatted-name "(GMT-04:00) La Paz",
    :formatted-offset "-04:00",
    :abbreviation "-04",
    :identifier "America/La_Paz"}
   {:name "Eastern Time (US & Canada)",
    :utc-offset -14400,
    :formatted-name "(GMT-04:00) Eastern Time (US & Canada)",
    :formatted-offset "-04:00",
    :abbreviation "EDT",
    :identifier "America/New_York"}
   {:name "Santiago",
    :utc-offset -14400,
    :formatted-name "(GMT-04:00) Santiago",
    :formatted-offset "-04:00",
    :abbreviation "-04",
    :identifier "America/Santiago"}
   {:name "Buenos Aires",
    :utc-offset -10800,
    :formatted-name "(GMT-03:00) Buenos Aires",
    :formatted-offset "-03:00",
    :abbreviation "-03",
    :identifier "America/Argentina/Buenos_Aires"}
   {:name "Georgetown",
    :utc-offset -10800,
    :formatted-name "(GMT-03:00) Georgetown",
    :formatted-offset "-03:00",
    :abbreviation "-03",
    :identifier "America/Argentina/San_Juan"}
   {:name "Atlantic Time (Canada)",
    :utc-offset -10800,
    :formatted-name "(GMT-03:00) Atlantic Time (Canada)",
    :formatted-offset "-03:00",
    :abbreviation "ADT",
    :identifier "America/Halifax"}
   {:name "Brasilia",
    :utc-offset -10800,
    :formatted-name "(GMT-03:00) Brasilia",
    :formatted-offset "-03:00",
    :abbreviation "-03",
    :identifier "America/Sao_Paulo"}
   {:name "Newfoundland",
    :utc-offset -9000,
    :formatted-name "(GMT-02:30) Newfoundland",
    :formatted-offset "-02:30",
    :abbreviation "NDT",
    :identifier "America/St_Johns"}
   {:name "Greenland",
    :utc-offset -7200,
    :formatted-name "(GMT-02:00) Greenland",
    :formatted-offset "-02:00",
    :abbreviation "-02",
    :identifier "America/Godthab"}
   {:name "Mid-Atlantic",
    :utc-offset -7200,
    :formatted-name "(GMT-02:00) Mid-Atlantic",
    :formatted-offset "-02:00",
    :abbreviation "-02",
    :identifier "Atlantic/South_Georgia"}
   {:name "Cape Verde Is.",
    :utc-offset -3600,
    :formatted-name "(GMT-01:00) Cape Verde Is.",
    :formatted-offset "-01:00",
    :abbreviation "-01",
    :identifier "Atlantic/Cape_Verde"}
   {:name "Monrovia",
    :utc-offset 0,
    :formatted-name "(GMT+00:00) Monrovia",
    :formatted-offset "+00:00",
    :abbreviation "GMT",
    :identifier "Africa/Monrovia"}
   {:name "Azores",
    :utc-offset 0,
    :formatted-name "(GMT+00:00) Azores",
    :formatted-offset "+00:00",
    :abbreviation "+00",
    :identifier "Atlantic/Azores"}
   {:name "UTC",
    :utc-offset 0,
    :formatted-name "(GMT+00:00) UTC",
    :formatted-offset "+00:00",
    :abbreviation "UTC",
    :identifier "Etc/UTC"}
   {:name "West Central Africa",
    :utc-offset 3600,
    :formatted-name "(GMT+01:00) West Central Africa",
    :formatted-offset "+01:00",
    :abbreviation "CET",
    :identifier "Africa/Algiers"}
   {:name "Casablanca",
    :utc-offset 3600,
    :formatted-name "(GMT+01:00) Casablanca",
    :formatted-offset "+01:00",
    :abbreviation "WEST",
    :identifier "Africa/Casablanca"}
   {:name "Dublin",
    :utc-offset 3600,
    :formatted-name "(GMT+01:00) Dublin",
    :formatted-offset "+01:00",
    :abbreviation "IST",
    :identifier "Europe/Dublin"}
   {:name "Edinburgh",
    :utc-offset 3600,
    :formatted-name "(GMT+01:00) Edinburgh",
    :formatted-offset "+01:00",
    :abbreviation "IST",
    :identifier "Europe/Dublin"}
   {:name "Lisbon",
    :utc-offset 3600,
    :formatted-name "(GMT+01:00) Lisbon",
    :formatted-offset "+01:00",
    :abbreviation "WEST",
    :identifier "Europe/Lisbon"}
   {:name "London",
    :utc-offset 3600,
    :formatted-name "(GMT+01:00) London",
    :formatted-offset "+01:00",
    :abbreviation "BST",
    :identifier "Europe/London"}
   {:name "Cairo",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Cairo",
    :formatted-offset "+02:00",
    :abbreviation "EET",
    :identifier "Africa/Cairo"}
   {:name "Harare",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Harare",
    :formatted-offset "+02:00",
    :abbreviation "CAT",
    :identifier "Africa/Harare"}
   {:name "Pretoria",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Pretoria",
    :formatted-offset "+02:00",
    :abbreviation "SAST",
    :identifier "Africa/Johannesburg"}
   {:name "Amsterdam",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Amsterdam",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Amsterdam"}
   {:name "Belgrade",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Belgrade",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Belgrade"}
   {:name "Berlin",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Berlin",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Berlin"}
   {:name "Bern",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Bern",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Berlin"}
   {:name "Bratislava",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Bratislava",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Bratislava"}
   {:name "Brussels",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Brussels",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Brussels"}
   {:name "Budapest",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Budapest",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Budapest"}
   {:name "Copenhagen",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Copenhagen",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Copenhagen"}
   {:name "Ljubljana",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Ljubljana",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Ljubljana"}
   {:name "Madrid",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Madrid",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Madrid"}
   {:name "Paris",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Paris",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Paris"}
   {:name "Prague",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Prague",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Prague"}
   {:name "Rome",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Rome",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Rome"}
   {:name "Sarajevo",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Sarajevo",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Sarajevo"}
   {:name "Skopje",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Skopje",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Skopje"}
   {:name "Stockholm",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Stockholm",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Stockholm"}
   {:name "Vienna",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Vienna",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Vienna"}
   {:name "Warsaw",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Warsaw",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Warsaw"}
   {:name "Zagreb",
    :utc-offset 7200,
    :formatted-name "(GMT+02:00) Zagreb",
    :formatted-offset "+02:00",
    :abbreviation "CEST",
    :identifier "Europe/Zagreb"}
   {:name "Nairobi",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Nairobi",
    :formatted-offset "+03:00",
    :abbreviation "EAT",
    :identifier "Africa/Nairobi"}
   {:name "Baghdad",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Baghdad",
    :formatted-offset "+03:00",
    :abbreviation "+03",
    :identifier "Asia/Baghdad"}
   {:name "Jerusalem",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Jerusalem",
    :formatted-offset "+03:00",
    :abbreviation "IDT",
    :identifier "Asia/Jerusalem"}
   {:name "Kuwait",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Kuwait",
    :formatted-offset "+03:00",
    :abbreviation "+03",
    :identifier "Asia/Kuwait"}
   {:name "Riyadh",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Riyadh",
    :formatted-offset "+03:00",
    :abbreviation "+03",
    :identifier "Asia/Riyadh"}
   {:name "Athens",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Athens",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Athens"}
   {:name "Bucharest",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Bucharest",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Bucharest"}
   {:name "Helsinki",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Helsinki",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Helsinki"}
   {:name "Istanbul",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Istanbul",
    :formatted-offset "+03:00",
    :abbreviation "+03",
    :identifier "Europe/Istanbul"}
   {:name "Kyev",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Kyev",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Kiev"}
   {:name "Minsk",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Minsk",
    :formatted-offset "+03:00",
    :abbreviation "+03",
    :identifier "Europe/Minsk"}
   {:name "Moscow",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Moscow",
    :formatted-offset "+03:00",
    :abbreviation "MSK",
    :identifier "Europe/Moscow"}
   {:name "St. Petersburg",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) St. Petersburg",
    :formatted-offset "+03:00",
    :abbreviation "MSK",
    :identifier "Europe/Moscow"}
   {:name "Volgograd",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Volgograd",
    :formatted-offset "+03:00",
    :abbreviation "MSK",
    :identifier "Europe/Moscow"}
   {:name "Riga",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Riga",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Riga"}
   {:name "Sofia",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Sofia",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Sofia"}
   {:name "Tallinn",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Tallinn",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Tallinn"}
   {:name "Vilnius",
    :utc-offset 10800,
    :formatted-name "(GMT+03:00) Vilnius",
    :formatted-offset "+03:00",
    :abbreviation "EEST",
    :identifier "Europe/Vilnius"}
   {:name "Baku",
    :utc-offset 14400,
    :formatted-name "(GMT+04:00) Baku",
    :formatted-offset "+04:00",
    :abbreviation "+04",
    :identifier "Asia/Baku"}
   {:name "Abu Dhabi",
    :utc-offset 14400,
    :formatted-name "(GMT+04:00) Abu Dhabi",
    :formatted-offset "+04:00",
    :abbreviation "+04",
    :identifier "Asia/Muscat"}
   {:name "Muscat",
    :utc-offset 14400,
    :formatted-name "(GMT+04:00) Muscat",
    :formatted-offset "+04:00",
    :abbreviation "+04",
    :identifier "Asia/Muscat"}
   {:name "Tbilisi",
    :utc-offset 14400,
    :formatted-name "(GMT+04:00) Tbilisi",
    :formatted-offset "+04:00",
    :abbreviation "+04",
    :identifier "Asia/Tbilisi"}
   {:name "Yerevan",
    :utc-offset 14400,
    :formatted-name "(GMT+04:00) Yerevan",
    :formatted-offset "+04:00",
    :abbreviation "+04",
    :identifier "Asia/Yerevan"}
   {:name "Kabul",
    :utc-offset 16200,
    :formatted-name "(GMT+04:30) Kabul",
    :formatted-offset "+04:30",
    :abbreviation "+0430",
    :identifier "Asia/Kabul"}
   {:name "Tehran",
    :utc-offset 16200,
    :formatted-name "(GMT+04:30) Tehran",
    :formatted-offset "+04:30",
    :abbreviation "+0430",
    :identifier "Asia/Tehran"}
   {:name "Islamabad",
    :utc-offset 18000,
    :formatted-name "(GMT+05:00) Islamabad",
    :formatted-offset "+05:00",
    :abbreviation "PKT",
    :identifier "Asia/Karachi"}
   {:name "Karachi",
    :utc-offset 18000,
    :formatted-name "(GMT+05:00) Karachi",
    :formatted-offset "+05:00",
    :abbreviation "PKT",
    :identifier "Asia/Karachi"}
   {:name "Tashkent",
    :utc-offset 18000,
    :formatted-name "(GMT+05:00) Tashkent",
    :formatted-offset "+05:00",
    :abbreviation "+05",
    :identifier "Asia/Tashkent"}
   {:name "Ekaterinburg",
    :utc-offset 18000,
    :formatted-name "(GMT+05:00) Ekaterinburg",
    :formatted-offset "+05:00",
    :abbreviation "+05",
    :identifier "Asia/Yekaterinburg"}
   {:name "Sri Jayawardenepura",
    :utc-offset 19800,
    :formatted-name "(GMT+05:30) Sri Jayawardenepura",
    :formatted-offset "+05:30",
    :abbreviation "+0530",
    :identifier "Asia/Colombo"}
   {:name "Chennai",
    :utc-offset 19800,
    :formatted-name "(GMT+05:30) Chennai",
    :formatted-offset "+05:30",
    :abbreviation "IST",
    :identifier "Asia/Kolkata"}
   {:name "Kolkata",
    :utc-offset 19800,
    :formatted-name "(GMT+05:30) Kolkata",
    :formatted-offset "+05:30",
    :abbreviation "IST",
    :identifier "Asia/Kolkata"}
   {:name "Mumbai",
    :utc-offset 19800,
    :formatted-name "(GMT+05:30) Mumbai",
    :formatted-offset "+05:30",
    :abbreviation "IST",
    :identifier "Asia/Kolkata"}
   {:name "New Delhi",
    :utc-offset 19800,
    :formatted-name "(GMT+05:30) New Delhi",
    :formatted-offset "+05:30",
    :abbreviation "IST",
    :identifier "Asia/Kolkata"}
   {:name "Kathmandu",
    :utc-offset 20700,
    :formatted-name "(GMT+05:45) Kathmandu",
    :formatted-offset "+05:45",
    :abbreviation "+0545",
    :identifier "Asia/Katmandu"}
   {:name "Almaty",
    :utc-offset 21600,
    :formatted-name "(GMT+06:00) Almaty",
    :formatted-offset "+06:00",
    :abbreviation "+06",
    :identifier "Asia/Almaty"}
   {:name "Astana",
    :utc-offset 21600,
    :formatted-name "(GMT+06:00) Astana",
    :formatted-offset "+06:00",
    :abbreviation "+06",
    :identifier "Asia/Dhaka"}
   {:name "Dhaka",
    :utc-offset 21600,
    :formatted-name "(GMT+06:00) Dhaka",
    :formatted-offset "+06:00",
    :abbreviation "+06",
    :identifier "Asia/Dhaka"}
   {:name "Urumqi",
    :utc-offset 21600,
    :formatted-name "(GMT+06:00) Urumqi",
    :formatted-offset "+06:00",
    :abbreviation "+06",
    :identifier "Asia/Urumqi"}
   {:name "Rangoon",
    :utc-offset 23400,
    :formatted-name "(GMT+06:30) Rangoon",
    :formatted-offset "+06:30",
    :abbreviation "+0630",
    :identifier "Asia/Rangoon"}
   {:name "Bangkok",
    :utc-offset 25200,
    :formatted-name "(GMT+07:00) Bangkok",
    :formatted-offset "+07:00",
    :abbreviation "+07",
    :identifier "Asia/Bangkok"}
   {:name "Hanoi",
    :utc-offset 25200,
    :formatted-name "(GMT+07:00) Hanoi",
    :formatted-offset "+07:00",
    :abbreviation "+07",
    :identifier "Asia/Bangkok"}
   {:name "Jakarta",
    :utc-offset 25200,
    :formatted-name "(GMT+07:00) Jakarta",
    :formatted-offset "+07:00",
    :abbreviation "WIB",
    :identifier "Asia/Jakarta"}
   {:name "Krasnoyarsk",
    :utc-offset 25200,
    :formatted-name "(GMT+07:00) Krasnoyarsk",
    :formatted-offset "+07:00",
    :abbreviation "+07",
    :identifier "Asia/Krasnoyarsk"}
   {:name "Novosibirsk",
    :utc-offset 25200,
    :formatted-name "(GMT+07:00) Novosibirsk",
    :formatted-offset "+07:00",
    :abbreviation "+07",
    :identifier "Asia/Novosibirsk"}
   {:name "Chongqing",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Chongqing",
    :formatted-offset "+08:00",
    :abbreviation "CST",
    :identifier "Asia/Chongqing"}
   {:name "Hong Kong",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Hong Kong",
    :formatted-offset "+08:00",
    :abbreviation "HKT",
    :identifier "Asia/Hong_Kong"}
   {:name "Irkutsk",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Irkutsk",
    :formatted-offset "+08:00",
    :abbreviation "+08",
    :identifier "Asia/Irkutsk"}
   {:name "Kuala Lumpur",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Kuala Lumpur",
    :formatted-offset "+08:00",
    :abbreviation "+08",
    :identifier "Asia/Kuala_Lumpur"}
   {:name "Beijing",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Beijing",
    :formatted-offset "+08:00",
    :abbreviation "CST",
    :identifier "Asia/Shanghai"}
   {:name "Singapore",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Singapore",
    :formatted-offset "+08:00",
    :abbreviation "+08",
    :identifier "Asia/Singapore"}
   {:name "Taipei",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Taipei",
    :formatted-offset "+08:00",
    :abbreviation "CST",
    :identifier "Asia/Taipei"}
   {:name "Ulaan Bataar",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Ulaan Bataar",
    :formatted-offset "+08:00",
    :abbreviation "+08",
    :identifier "Asia/Ulaanbaatar"}
   {:name "Perth",
    :utc-offset 28800,
    :formatted-name "(GMT+08:00) Perth",
    :formatted-offset "+08:00",
    :abbreviation "AWST",
    :identifier "Australia/Perth"}
   {:name "Seoul",
    :utc-offset 32400,
    :formatted-name "(GMT+09:00) Seoul",
    :formatted-offset "+09:00",
    :abbreviation "KST",
    :identifier "Asia/Seoul"}
   {:name "Osaka",
    :utc-offset 32400,
    :formatted-name "(GMT+09:00) Osaka",
    :formatted-offset "+09:00",
    :abbreviation "JST",
    :identifier "Asia/Tokyo"}
   {:name "Sapporo",
    :utc-offset 32400,
    :formatted-name "(GMT+09:00) Sapporo",
    :formatted-offset "+09:00",
    :abbreviation "JST",
    :identifier "Asia/Tokyo"}
   {:name "Tokyo",
    :utc-offset 32400,
    :formatted-name "(GMT+09:00) Tokyo",
    :formatted-offset "+09:00",
    :abbreviation "JST",
    :identifier "Asia/Tokyo"}
   {:name "Yakutsk",
    :utc-offset 32400,
    :formatted-name "(GMT+09:00) Yakutsk",
    :formatted-offset "+09:00",
    :abbreviation "+09",
    :identifier "Asia/Yakutsk"}
   {:name "Adelaide",
    :utc-offset 34200,
    :formatted-name "(GMT+09:30) Adelaide",
    :formatted-offset "+09:30",
    :abbreviation "ACST",
    :identifier "Australia/Adelaide"}
   {:name "Darwin",
    :utc-offset 34200,
    :formatted-name "(GMT+09:30) Darwin",
    :formatted-offset "+09:30",
    :abbreviation "ACST",
    :identifier "Australia/Darwin"}
   {:name "Vladivostok",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Vladivostok",
    :formatted-offset "+10:00",
    :abbreviation "+10",
    :identifier "Asia/Vladivostok"}
   {:name "Brisbane",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Brisbane",
    :formatted-offset "+10:00",
    :abbreviation "AEST",
    :identifier "Australia/Brisbane"}
   {:name "Hobart",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Hobart",
    :formatted-offset "+10:00",
    :abbreviation "AEST",
    :identifier "Australia/Hobart"}
   {:name "Canberra",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Canberra",
    :formatted-offset "+10:00",
    :abbreviation "AEST",
    :identifier "Australia/Melbourne"}
   {:name "Melbourne",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Melbourne",
    :formatted-offset "+10:00",
    :abbreviation "AEST",
    :identifier "Australia/Melbourne"}
   {:name "Sydney",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Sydney",
    :formatted-offset "+10:00",
    :abbreviation "AEST",
    :identifier "Australia/Sydney"}
   {:name "Guam",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Guam",
    :formatted-offset "+10:00",
    :abbreviation "ChST",
    :identifier "Pacific/Guam"}
   {:name "Port Moresby",
    :utc-offset 36000,
    :formatted-name "(GMT+10:00) Port Moresby",
    :formatted-offset "+10:00",
    :abbreviation "+10",
    :identifier "Pacific/Port_Moresby"}
   {:name "Magadan",
    :utc-offset 39600,
    :formatted-name "(GMT+11:00) Magadan",
    :formatted-offset "+11:00",
    :abbreviation "+11",
    :identifier "Asia/Magadan"}
   {:name "Solomon Is.",
    :utc-offset 39600,
    :formatted-name "(GMT+11:00) Solomon Is.",
    :formatted-offset "+11:00",
    :abbreviation "+11",
    :identifier "Asia/Magadan"}
   {:name "New Caledonia",
    :utc-offset 39600,
    :formatted-name "(GMT+11:00) New Caledonia",
    :formatted-offset "+11:00",
    :abbreviation "+11",
    :identifier "Pacific/Noumea"}
   {:name "Kamchatka",
    :utc-offset 43200,
    :formatted-name "(GMT+12:00) Kamchatka",
    :formatted-offset "+12:00",
    :abbreviation "+12",
    :identifier "Asia/Kamchatka"}
   {:name "Auckland",
    :utc-offset 43200,
    :formatted-name "(GMT+12:00) Auckland",
    :formatted-offset "+12:00",
    :abbreviation "NZST",
    :identifier "Pacific/Auckland"}
   {:name "Wellington",
    :utc-offset 43200,
    :formatted-name "(GMT+12:00) Wellington",
    :formatted-offset "+12:00",
    :abbreviation "NZST",
    :identifier "Pacific/Auckland"}
   {:name "Fiji",
    :utc-offset 43200,
    :formatted-name "(GMT+12:00) Fiji",
    :formatted-offset "+12:00",
    :abbreviation "+12",
    :identifier "Pacific/Fiji"}
   {:name "Marshall Is.",
    :utc-offset 43200,
    :formatted-name "(GMT+12:00) Marshall Is.",
    :formatted-offset "+12:00",
    :abbreviation "+12",
    :identifier "Pacific/Majuro"}
   {:name "Nuku'alofa",
    :utc-offset 46800,
    :formatted-name "(GMT+13:00) Nuku'alofa",
    :formatted-offset "+13:00",
    :abbreviation "+13",
    :identifier "Pacific/Tongatapu"}])

(def timezone-name
  (gen/one-of (mapv (comp gen/return :name) time-zones)))

(def user
  (gen/let [mycurrency currency-with-country-code]
    (gen/hash-map
     :id (gen/large-integer* {:min 0})
     :name gen/string-ascii
     :login gen/string-ascii
     :base-currency-code (gen/fmap
                          (comp sg/lower-case str)
                          (gen/return mycurrency))
     ;; close, but they aren't using a uuid
     :avatar-url (gen/fmap
                  #(str "https://secure.gravatar.com/avatar/" % "?d=404")
                  gen/uuid)

     :country-code (gen/one-of (mapv gen/return (country-codes mycurrency)))

     :tell-a-friend-access (gen/one-of [(gen/return nil) gen/string-ascii])
     :available-budgets (gen/large-integer* {:min 0 :max 9976})
     :available-accounts (gen/large-integer* {:min 0 :max 12})
     :week-start-day (gen/large-integer* {:min 0 :max 6})

     :created-at (string-date :date-time-no-ms)
     :updated-at (string-date :date-time-no-ms)
     :last-logged-in-at (string-date :date-time-no-ms)
     :last-activity-at (string-date :date-time-no-ms)

     :forecast-last-updated-at (string-date :date-time-no-ms)
     :forecast-last-accessed-at (string-date :date-time-no-ms)
     :forecast-start-date (string-date :year-month-day)
     :forecast-end-date (string-date :year-month-day)
     :forecast-needs-recalculate gen/boolean
     :forecast-defer-recalculate gen/boolean

     :is-reviewing-transactions gen/boolean
     :always-show-base-currency gen/boolean
     :using-multiple-currencies gen/boolean
     :allowed-data-feeds gen/boolean
     :using-feed-support-requests gen/boolean

     :time-zone timezone-name)))

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

(defn institution [created-at updated-at]
  (gen/hash-map
   :id (gen/large-integer* {:min 0})
   :title gen/string-ascii
   :currency-code (gen/fmap (comp sg/lower-case str) currency)
   :created-at (string-date created-at :date-time-no-ms)
   :updated-at (string-date updated-at :date-time-no-ms)))

(defn transaction-account [user]
  (gen/let [[t1 t2 t3 t4] (order t/before?
                                 (gen/tuple (date-time) (date-time)
                                            (date-time) (date-time)))
            acct-currency currency]
    (gen/hash-map
     :institution (institution t1 t3)
     :id (gen/large-integer* {:min 0})
     :title gen/string-ascii
     :number gen/string-ascii
     :account-id (gen/large-integer* {:min 0})
     :currency-code (gen/fmap (comp sg/lower-case str) (gen/return acct-currency))
     :created-at (string-date t2 :date-time-no-ms)
     :updated-at (string-date t4 :date-time-no-ms)
     :starting-balance-date (string-date :year-month-day)
     :current-balance-date (string-date :year-month-day)
     :has-safe-balance-adjustment gen/boolean
     :is-net-worth gen/boolean
     :offline gen/boolean
     :type (gen/one-of [(gen/return "property")
                        (gen/return "vehicle")
                        (gen/return "stocks")
                        (gen/return "credit")
                        (gen/return "bank")])

     :starting-balance (gen/fmap money->bigdec (money* acct-currency {}))
     :current-balance (gen/fmap money->bigdec (money* acct-currency {}))
     :current-balance-in-base-currency (gen/fmap money->bigdec (money* (mc/for-code (sg/upper-case (:base-currency-code user))) {}))

     ;; incomplete
     :current-balance-source (gen/one-of [(gen/return "scenario_only_balance") (gen/return "data_feed")])
     :data-feeds-balance-type (gen/return "balance") ;; need other options
     :data-feeds-connection-id (gen/one-of [(gen/return nil) gen/string-ascii])
     :data-feeds-account-id (gen/one-of [(gen/return nil) gen/string-ascii])
     :latest-feed-name (gen/one-of [(gen/return nil) gen/string-ascii])

     ;; incorrect
     :current-balance-exchange-rate (gen/return nil)
     :safe-balance-in-base-currency (gen/return nil)
     :safe-balance (gen/return nil))))

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

(defn distinct-vals [maps]
  (reduce
   (fn [a b]
     (reduce-kv
      (fn [a k v]
        (-> a
            (update k conj v)
            (update k distinct)))
      a b))
   {} maps))

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

(defn transaction [user]
  (gen/let [[t1 t2 t3] (order t/before? (gen/tuple (date-time) (date-time) (date-time)))
            acct (transaction-account user)]
    (gen/hash-map
     :id (gen/large-integer* {:min 0})
     :payee gen/string-ascii
     :original-payee gen/string-ascii

     :amount (gen/fmap money->bigdec (money* (mc/for-code (sg/upper-case (:currency-code acct))) {}))
     :closing-balance (gen/fmap money->bigdec (money* (mc/for-code (sg/upper-case (:currency-code acct))) {}))
     :amount-in-base-currency (gen/fmap money->bigdec (money* (mc/for-code (sg/upper-case (:base-currency-code user))) {}))

     :created-at (string-date t1 :date-time-no-ms)
     :updated-at (string-date t2 :date-time-no-ms)
     :date (string-date t3 :year-month-day)

     :transaction-account (gen/return acct)
     :category category

     :labels (gen/vector gen/string-ascii)
     :upload-source (gen/return "data_feed")
     :type (gen/one-of [(gen/return "debit") (gen/return "credit")])
     :note (gen/one-of [(gen/return nil) gen/string-ascii])
     :status (gen/return "posted")
     :is-transfer (gen/one-of [(gen/return nil) gen/boolean])
     :cheque-number (gen/one-of [(gen/return nil) (gen/large-integer* {:min 1000 :max 9999})])
     :needs-review gen/boolean
     :memo (gen/return nil))))
