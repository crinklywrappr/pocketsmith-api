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
                      [format :as f]
                      [types :as ts]))
  (:import [org.joda.money CurrencyUnit]
           [org.joda.time DateTime]))

(def link-regex #"<([^>]+)>; rel=\"(first|next|last)\"")
(def iso-8601 "yyyy-MM-dd'T'HH:mm:ssZ")

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
      (if (contains? opts :query-params)
        (fetch-one token key (dissoc opts :query-params))
        (fetch-one token key opts)))))

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

(defn error-response?
  "Predicate for determining if an element
  represents a network error or parse error"
  [response]
  (and (contains? response :status)
       (contains? response :headers)
       (contains? response :body)
       (contains? response :request)))

(defn convert-identifier-for-timezone [timezone]
  (update timezone :identifier t/time-zone-for-id))

(defn minify-timezone [timezone]
  (select-keys timezone [:name :identifier]))

(defn time-zones* [key]
  (fetch-many
   "https://api.pocketsmith.com/v2/time_zones"
   key {:throw-exceptions true
        :query-params {:per_page 100}}))

(defn time-zones
  "Returns pocketsmith timezone information.
  May throw an ExceptionInfo."
  [key & {:keys [convert? minify?]}]
  (cond->> (time-zones* key)
    convert? (r/map convert-identifier-for-timezone)
    minify? (r/map minify-timezone)))

(defn code->currency [x]
  (mc/for-code (sg/upper-case x)))

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

(defn convert-currency-code-on-user [user]
  (if (error-response? user)
    user
    (update user :base-currency-code code->currency)))

(defn convert-timezone-on-user [time-zones user]
  (if (error-response? user)
    user
    (update user :time-zone (partial get time-zones))))

(defn minify-user [user]
  (if (error-response? user)
    user
    (select-keys user [:name :login :email :week-start-day
                       :id :base-currency-code :time-zone])))

(defn authorized-user
  "Returns the authorized user.
  May throw an ExceptionInfo if `convert?` is `true`."
  [key & {:keys [convert? minify?]}]
  (let [time-zones (when convert?
                     (->> (time-zones key :convert? true :minify? true)
                          (r/map (comp vec vals))
                          (into {})))]
    (letfn [(get-page [{:keys [status body] :as response}]
              (if (== status 200) body response))]
      (cond->> (get-page (fetch-one "https://api.pocketsmith.com/v2/me" key {}))
        convert? convert-datetime-on-user
        convert? convert-currency-code-on-user
        (seq time-zones) (convert-timezone-on-user time-zones)
        minify? minify-user))))

(defn by-name-or-title [name-or-title xs]
  (some
   (fn search-fn [x]
     (when (or (= (:name x) name-or-title)
               (= (:title x) name-or-title))
       x)) xs))

(defn bigdec? [x]
  (instance? BigDecimal x))

(defn currency? [x]
  (instance? CurrencyUnit x))

(defn get-decimal-places [^CurrencyUnit x]
  (.getDecimalPlaces x))

(defn move-point-right [^BigDecimal x n]
  (.movePointRight x n))

(defn amount->money
  "assumes `code` satisfies the `currency?` predicate"
  [amount code]
  (if (bigdec? amount)
    (->> code get-decimal-places
         (move-point-right amount)
         str Long/parseLong
         (ma/of-minor code))
    amount))

(defn long->bigdec [x]
  (if (instance? Long x) (clojure.core/bigdec x) x))

(defn ensure-bigdec-values-on-account* [account]
  (-> account
      (update :starting-balance long->bigdec)
      (update :current-balance-in-base-currency long->bigdec)
      (update :current-balance long->bigdec)
      (update :safe-balance-in-base-currency long->bigdec)
      (update :safe-balance long->bigdec)))

(defn ensure-bigdec-values-on-account [account]
  (if (error-response? account)
    account
    (ensure-bigdec-values-on-account* account)))

(defn convert-currencies-on-account* [account]
  (-> account
      (update-in [:institution :currency-code] code->currency)
      (update :currency-code code->currency)))

(defn convert-currencies-on-account [account]
  (if (error-response? account)
    account
    (convert-currencies-on-account* account)))

(defn convert-amounts-on-account* [user account]
  (let [currency-code (:currency-code account)
        base-code (:base-currency-code user)]
    (-> account
        (update :starting-balance amount->money currency-code)
        (update :current-balance-in-base-currency amount->money base-code)
        (update :current-balance amount->money currency-code)
        (update :safe-balance-in-base-currency amount->money base-code)
        (update :safe-balance amount->money currency-code))))

(defn convert-amounts-on-account [user account]
  (if (error-response? account)
    account
    (convert-amounts-on-account* user account)))

(defn convert-datetime-on-account* [account]
  (-> account
      (update-in [:institution :created-at] parse-datetime :date-time-no-ms)
      (update-in [:institution :updated-at] parse-datetime :date-time-no-ms)
      (update :created-at parse-datetime :date-time-no-ms)
      (update :updated-at parse-datetime :date-time-no-ms)
      (update :starting-balance-date parse-local-datetime :year-month-day)
      (update :current-balance-date parse-local-datetime :year-month-day)))

(defn convert-datetime-on-account [account]
  (if (error-response? account)
    account
    (convert-datetime-on-account* account)))

(defn minify-account* [account]
  (select-keys account [:id :name :type
                        :starting-balance-date
                        :starting-balance
                        :current-balance-date :current-balance
                        :currency-code]))

(defn minify-account [account]
  (if (error-response? account)
    account
    (minify-account* account)))

(defn accounts
  "Returns TransactionAccounts.
  Expects `base-currency-code` to be a Joda Currency object.
  Throws an AssertionError if using `convert?` and that requirement isn't satisfied."
  [key user & {:keys [convert? minify?]}]
  {:pre [(or (and convert? (currency? (:base-currency-code user))) (not convert?))]}
  (cond->>
      (->>
       (fetch-many
        (render "https://api.pocketsmith.com/v2/users/{{id}}/transaction_accounts" user)
        key {:query-params {:per_page 100}})
       (r/map ensure-bigdec-values-on-account))
    convert? (r/map (comp convert-datetime-on-account
                      (partial convert-amounts-on-account user)
                      convert-currencies-on-account))
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
     (assoc node :children children))
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

(defn categories
  "Returns Categories.  Should not throw an exception.
  If there was a network problem, or a parse error,
  the last element will satisfy `error-response?`."
  [key user & {:keys [flatten? normalize? convert? minify?]}]
  (cond->>
      (fetch-many
       (render "https://api.pocketsmith.com/v2/users/{{id}}/categories" user)
       key {:query-params {:per_page 100}})
    convert? (r/map (partial modify-categories convert-datetime-on-category))
    minify? (r/map (partial modify-categories minify-category))
    (or flatten? normalize?) (r/mapcat flatten-category)
    normalize? (r/map normalize-category)))

(defn ensure-bigdec-values-on-transaction [transaction]
  (if (error-response? transaction)
    transaction
    (-> transaction
        (update :amount bigdec)
        (update :amount-in-base-currency bigdec)
        (update :closing-balance bigdec)
        (update :transaction-account ensure-bigdec-values-on-account*))))

(defn convert-currencies-on-transaction [transaction]
  (if (error-response? transaction)
    transaction
    (update transaction :transaction-account convert-currencies-on-account*)))

(defn convert-amounts-on-transaction [user transaction]
  (if (error-response? transaction)
    transaction
    (let [currency-code (-> transaction :transaction-account :currency-code)
          base-code (:base-currency-code user)]
      (-> transaction
          (update :amount amount->money currency-code)
          (update :amount-in-base-currency amount->money base-code)
          (update :closing-balance amount->money currency-code)
          (update :transaction-account (partial convert-amounts-on-account* user))))))

(defn convert-datetime-on-transaction [transaction]
  (if (error-response? transaction)
    transaction
    (-> transaction
        (update :transaction-account convert-datetime-on-account*)
        (update :created-at parse-datetime :date-time-no-ms)
        (update :updated-at parse-datetime :date-time-no-ms)
        (update :date parse-local-datetime :year-month-day))))

(defn normalize-transaction [transaction]
  (if (error-response? transaction)
    transaction
    (-> transaction
        (assoc :transaction-account-id (-> transaction :transaction-account :id)
               :category-id (-> transaction :category :id))
        (dissoc :transaction-account :category))))

(defn minify-transaction [transaction]
  (if (error-response? transaction)
    transaction
    (-> transaction
        (select-keys [:id :payee :date :amount
                      :amount-in-base-currency
                      :closing-balance
                      :transaction-account
                      :category :is-transfer])
        (update :transaction-account minify-account*)
        (update :category minify-category))))

(defn get-transactions*
  [key user uri query-params {:keys [normalize? convert? minify?]}]
  {:pre [(or (and convert? (currency? (:base-currency-code user))) (not convert?))]}
  (cond->>
      (->>
       {:query-params (merge {:per_page 100} query-params)}
       (fetch-many uri key)
       (r/map ensure-bigdec-values-on-transaction))
    convert? (r/map (comp convert-datetime-on-transaction
                          (partial convert-amounts-on-transaction user)
                          convert-currencies-on-transaction))
    minify? (r/map minify-transaction)
    normalize? (r/map normalize-transaction)))

(defn user-transactions
  "Returns Transactions.  Should not throw an exception.
  If there was a network problem, or a parse error,
  the last element will satisfy `error-response?`."
  [key user query-params & {:keys [normalize? convert? minify?] :as opts}]
  (get-transactions* key user (render "https://api.pocketsmith.com/v2/users/{{id}}/transactions" user) query-params opts))

(defn account-transactions
  "Returns Transactions.  Should not throw an exception.
  If there was a network problem, or a parse error,
  the last element will satisfy `error-response?`."
  [key user account query-params & {:keys [normalize? convert? minify?] :as opts}]
  (get-transactions* key user (render "https://api.pocketsmith.com/v2/transaction_accounts/{{id}}/transactions" account) query-params opts))

(defn category-transactions
  "Returns Transactions.  Should not throw an exception.
  If there was a network problem, or a parse error,
  the last element will satisfy `error-response?`."
  [key user category query-params & {:keys [normalize? convert? minify?] :as opts}]
  (get-transactions* key user (render "https://api.pocketsmith.com/v2/categories/{{id}}/transactions" category) query-params opts))

(defn time-zone [^DateTime date-time]
  (.getZone date-time))

(defn last-month
  "Returns a map with `:start_date` and `:end_date` keys.
  Intended to be passed to `transaction-query-params`.
  Will throw an assertion error if `user.time-zone`
  is not a Joda TimeZone"
  [{:keys [time-zone] :as user}]
  {:pre [(ts/time-zone? time-zone)]}
  (let [dt (t/to-time-zone (t/now) time-zone)
        a-month-ago (t/minus dt (t/months 1))
        end-of-month (t/minus dt (t/days (t/day dt)))
        year (t/year a-month-ago) month (t/month a-month-ago)]
    {:start-date (t/local-date year month 1)
     :end-date (t/local-date year month (t/day end-of-month))}))

(defn date-time? [x]
  (instance? DateTime x))

(defn transaction-query-params
  "formats a request map for the transaction functions.

  start_date, end_date => local date objects, inclusive
  updated_since => datetime object, with time-zone (use to-time-zone, or from-time-zone)
  type => :debit or :credit
  uncategorized?, needs_review? => boolean
  search => search string
  per_page => number between 10 & 100"
  [{:keys [start-date end-date updated-since search
           uncategorized? type needs-review? per-page]
    :or {per-page 100}}]
  (cond-> {}
    (ts/local-date? start-date) (assoc :start_date
                                       (f/unparse-local-date
                                        (f/formatter :year-month-day)
                                        start-date))
    (ts/local-date? end-date) (assoc :end_date
                                     (f/unparse-local-date
                                      (f/formatter :year-month-day)
                                      end-date))
    (date-time? updated-since) (assoc :updated_since
                                      (f/unparse
                                       (f/with-zone (f/formatter iso-8601)
                                         (time-zone updated-since))
                                       updated-since))
    (string? search) (assoc :search search)
    (boolean? uncategorized?) (assoc :uncategorised (if uncategorized? 1 0))
    (boolean? needs-review?) (assoc :needs_review (if needs-review? 1 0))
    (or (= type :debit) (= type :credit)) (assoc :type (name type))
    (number? per-page) (assoc :per_page
                              (cond
                                (< per-page 10) 10
                                (> per-page 100) 100
                                :else per-page))))
