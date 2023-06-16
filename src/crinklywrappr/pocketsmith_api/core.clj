(ns crinklywrappr.pocketsmith-api.core
  (:require [clojure.core.reducers :as r]
            [clojure.math :as m]
            [clojure.zip :as zip]
            [clojure.string :as sg]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [selmer.parser :refer [render]]
            (camel-snake-kebab [core :as csk]
                               [extras :as cske])
            (clojurewerkz.money [amounts :as ma]
                                [currencies :as mc]
                                [format :as mf])
            (clj-time [core :as t]
                      [local :as l]
                      [format :as f])))

(def link-regex #"<([^>]+)>; rel=\"(first|next|last)\"")

(defn parse-link [link]
  (reduce
   (fn [a [_ uri rel]]
     (assoc a (keyword rel) uri))
   {} (re-seq link-regex link)))

(defn request-map [key]
  {:accept :json
   :throw-exceptions false
   :headers {:X-Developer-Key key}})

(defn parse-body [{:keys [body] :as response}]
  (try
    (update response :body json/read-str :bigdec true :key-fn csk/->kebab-case-keyword)
    (catch Exception ex
      (assoc response :body body :parse-error (str (type ex))))))

(defn fetch-one [uri key opts]
  (->
   (client/get uri (merge (request-map key) opts))
   (select-keys [:status :headers :body])
   parse-body
   (update :headers (partial cske/transform-keys csk/->kebab-case-keyword))
   (assoc :request {:uri uri :key key :opts opts})))

(defn fetch-page [uri key opts]
  (fn [token]
    (if (nil? token)
      (fetch-one uri key opts)
      (fetch-one token key {}))))

(defn next-page [{:keys [status headers]}]
  (when (and (== status 200) (contains? headers :link))
    (-> headers :link parse-link :next)))

(defn get-page [{:keys [status body] :as response}]
  (if (or (contains? response :parse-error) (not= status 200))
    [response]
    body))

(defn fetch-many [uri key opts]
  (as-> uri $
    (fetch-page $ key opts)
    (iteration $ :vf get-page :kf next-page)
    (r/mapcat identity $)))

(defn parse-datetime [dt fmt]
  (if (and (string? dt) (seq dt))
    (f/parse (f/formatter fmt) dt)
    dt))

(defn parse-local-datetime [dt fmt]
  (if (and (string? dt) (seq dt))
    (f/parse-local (f/formatter fmt) dt)
    dt))

(defn error-response? [response]
  (and (contains? response :status)
       (contains? response :headers)
       (contains? response :body)
       (contains? response :request)))

(defn convert-datetime-on-user [user]
  (if (error-response? user)
    user
    (-> user
        (update :created-at parse-datetime :date-time-no-ms)
        (update :updated-at parse-datetime :date-time-no-ms)
        (update :last-logged-in-at parse-datetime :date-time-no-ms)
        (update :last-activity-at parse-datetime :date-time-no-ms)
        (update :forecast-last-accessed-at parse-datetime :date-time-no-ms)
        (update :forecast-last-updated-at parse-datetime :date-time-no-ms)
        (update :forecast-start-date parse-local-datetime :year-month-day)
        (update :forecast-end-date parse-local-datetime :year-month-day))))

(defn minify-user [user]
  (if (error-response? user)
    user
    (select-keys user [:id :name :login :email :base-currency-code :time-zone])))

(defn authorized-user [key & {:keys [convert? minify?]}]
  (letfn [(get-page [{:keys [status body] :as response}]
            (if (== status 200) body response))]
    (cond->> (get-page (fetch-one "https://api.pocketsmith.com/v2/me" key {}))
      convert? convert-datetime-on-user
      minify? minify-user)))

(defn by-name-or-title [name-or-title xs]
  (some
   (fn search-fn [x]
     (when (or (= (:name x) name-or-title)
               (= (:title x) name-or-title))
       x)) xs))

(defn bigdec? [x] (instance? BigDecimal x))

(defn amount->money [amount code]
  (if (and (bigdec? amount) (string? code) (seq code))
    (let [currency (mc/for-code (sg/upper-case code))]
      (->> currency .getDecimalPlaces
           (.movePointRight amount)
           str Long/parseLong
           (ma/of-minor currency)))
    amount))

(defn long->bigdec [x]
  (if (instance? Long x) (clojure.core/bigdec x) x))

(defn ensure-bigdec-values-on-account [account]
  (if (error-response? account)
    account
    (-> account
        (update :starting-balance long->bigdec)
        (update :current-balance-in-base-currency long->bigdec)
        (update :current-balance long->bigdec)
        (update :safe-balance-in-base-currency long->bigdec)
        (update :safe-balance long->bigdec))))

(defn convert-amounts-on-account [user account]
  (if (error-response? account)
    account
    (let [currency-code (:currency-code account)
          base-code (:base-currency-code user)]
      (-> account
          (update :starting-balance amount->money currency-code)
          (update :current-balance-in-base-currency amount->money base-code)
          (update :current-balance amount->money currency-code)
          (update :safe-balance-in-base-currency amount->money base-code)
          (update :safe-balance amount->money currency-code)))))

(defn convert-datetime-on-account [account]
  (if (error-response? account)
    account
    (-> account
        (update-in [:institution :created-at] parse-datetime :date-time-no-ms)
        (update-in [:institution :updated-at] parse-datetime :date-time-no-ms)
        (update :created-at parse-datetime :date-time-no-ms)
        (update :updated-at parse-datetime :date-time-no-ms)
        (update :starting-balance-date parse-local-datetime :year-month-day)
        (update :current-balance-date parse-local-datetime :year-month-day))))

(defn minify-account [account]
  (if (error-response? account)
    account
    (select-keys account [:id :name :type
                          :starting-balance-date
                          :starting-balance
                          :current-balance-date :current-balance
                          :currency-code])))

(defn accounts [key user & {:keys [convert? minify?]}]
  (cond->>
      (->>
       (fetch-many
        (render "https://api.pocketsmith.com/v2/users/{{id}}/transaction_accounts" user)
        key {})
       (r/map ensure-bigdec-values-on-account))
    convert? (r/map (comp convert-datetime-on-account
                      (partial convert-amounts-on-account user)))
    minify? (r/map minify-account)))

(defn category? [x]
  (and (contains? x :id)
       (or
        (contains? x :parents)
        (contains? x :children))))

(defn category-zip [category]
  (zip/zipper
   category? :children
   (fn [{:keys [id] :as node} children]
     (assoc node :children (mapv #(assoc % :parent-id id) children)))
   category))

(defn flatten-category [category]
  (if (error-response? category)
    [category]
    (letfn [(-children [x] (dissoc x :children))]
      (loop [acc [] z (category-zip category)]
        (if (zip/end? z)
          acc
          (if-let [node (zip/node z)]
            (recur
             (conj acc (-> node -children
                           (assoc :parents (mapv -children (zip/path z)))))
             (zip/next z))
            (recur acc (zip/next z))))))))

(defn normalize-category [category]
  (if (error-response? category)
    category
    (update category :parents (partial mapv :id))))

(defn convert-datetime-on-category [category]
  (-> category
      (update :created-at parse-datetime :date-time-no-ms)
      (update :updated-at parse-datetime :date-time-no-ms)))

(defn minify-category [category]
  (select-keys category [:id :title :children]))

(defn modify-category* [f loc]
  (if (category? (zip/node loc))
    (zip/edit loc f)
    loc))

(defn modify-categories [f category]
  (if (error-response? category)
    category
    (let [f (partial modify-category* f)]
      (loop [z (category-zip category)]
        (if (zip/end? z)
          (zip/root z)
          (recur (zip/next (f z))))))))

(defn categories [key user & {:keys [flatten? normalize? convert? minify?]}]
  (cond->>
      (fetch-many
       (render "https://api.pocketsmith.com/v2/users/{{id}}/categories" user)
       key {})
    convert? (r/map (partial modify-categories convert-datetime-on-category))
    minify? (r/map (partial modify-categories minify-category))
    (or flatten? normalize?) (r/mapcat flatten-category)
    normalize? (r/map normalize-category) ))

(defn ensure-bigdec-values-on-transaction [transaction]
  (-> transaction
      (update :amount bigdec)
      (update :amount-in-base-currency bigdec)
      (update :closing-balance bigdec)
      (update :transaction-account ensure-bigdec-values-on-account)))

(defn convert-amounts-on-transaction [user transaction]
  (let [currency-code (-> transaction :transaction-account :currency-code)
        base-code (:base-currency-code user)]
    (-> transaction
        (update :amount amount->money currency-code)
        (update :amount-in-base-currency amount->money base-code)
        (update :closing-balance amount->money currency-code)
        (update :transaction-account (partial convert-amounts-on-account user)))))

(defn normalize-transaction [transaction]
  (-> transaction
      (assoc :transaction-account-id (-> transaction :transaction-account :id)
             :category-id (-> transaction :category :id))
      (dissoc :transaction-account :category)))

(defn minify-transaction [transaction]
  (-> transaction
      (select-keys [:id :payee :date :amount
                    :amount-in-base-currency
                    :closing-balance
                    :transaction-account
                    :category])
      (update :transaction-account minify-account)
      (update :category minify-category)))

(defn user-transactions
  [key user query-params & {:keys [normalize? convert? minify?]}]
  (cond->>
      (->>
       (fetch-many
        (render "https://api.pocketsmith.com/v2/users/{{id}}/transactions" user)
        key {:query-params query-params})
       (r/map ensure-bigdec-values-on-transaction))
    convert? (r/map (partial convert-amounts-on-transaction user))
    minify? (r/map minify-transaction)
    normalize? (r/map normalize-transaction)))

(defn account-transactions
  [key user account query-params & {:keys [normalize? convert? minify?]}]
  (cond->>
      (fetch-many
       (render "https://api.pocketsmith.com/v2/transaction_accounts/{{id}}/transactions" account)
       key {:query-params query-params})
    convert? (r/map (partial convert-amounts-on-transaction user))
    minify? (r/map minify-transaction)
    normalize? (r/map normalize-transaction)))

(defn category-transactions
  [key user category query-params & {:keys [normalize? convert? minify?]}]
  (cond->>
      (fetch-many
       (render "https://api.pocketsmith.com/v2/categories/{{id}}/transactions" category)
       key {:query-params query-params})
    convert? (r/map (partial convert-amounts-on-transaction user))
    minify? (r/map minify-transaction)
    normalize? (r/map normalize-transaction)))
