# com.github.crinklywrappr/pocketsmith-api

Clojure library for interacting with the [Pocketsmith](https://www.pocketsmith.com/) REST API.  Small, opinionated, and hand-crafted.

# Coordinates

```clojure
com.github.crinklywrappr/pocketsmith-api {:mvn/version "1.0.37"}
```

# Usage

First, require the library and define your user key.  You can generate a key on the dashboard from your pocketsmith account.

```clojure
(require '[crinklywrappr.pocketsmith-api.core :as ps])

(def mykey "xxx")
```

Then, begin querying.

```clojure
(def myuser (ps/authorized-user mykey :convert? true :minify? true))
;=> {:name "John Doe",
     :login "my-username",
     :email "my-address@email.com",
     :week-start-day 0,
     :id 1111111,
     :base-currency-code #object[org.joda.money.CurrencyUnit 0x4d1b5405 "USD"],
     :time-zone #object[org.joda.time.tz.CachedDateTimeZone 0x468ce331 "America/Chicago"]}

(def mycategories (into [] (ps/categories mykey myuser :flatten? true :normalize? true :convert? true :minify? true)))
;=> [{:id XXX, :title "Charity", :parents []}
     {:id XXX, :title "Taxes", :parents []}
     {:id XXX, :title "Bank", :parents []}
     {:id XXX, :title "Deposit", :parents [XXX]}
     {:id XXX, :title "Interest", :parents [XXX]}
     {:id XXX, :title "Checks", :parents [XXX]}
     {:id XXX, :title "ATM", :parents [XXX]}
     {:id XXX, :title "Transfer", :parents [XXX]}
     {:id XXX, :title "Recurring Charge", :parents []}
     {:id XXX, :title "Credit Card", :parents [XXX]}
     {:id XXX, :title "Streaming", :parents [XXX]}
     {:id XXX, :title "Utility", :parents [XXX]}
     {:id XXX, :title "Food/Drink", :parents []}
     {:id XXX, :title "Restaurant", :parents [XXX]}
     {:id XXX, :title "Delivery", :parents [XXX]}
     {:id XXX, :title "Grocery", :parents [XXX]}
     {:id XXX, :title "Smokey", :parents []}
     {:id XXX, :title "Home/Car", :parents []}
     {:id XXX, :title "Health", :parents []}
     {:id XXX, :title "Fuel", :parents []}
     {:id XXX, :title "Toy", :parents []}
     {:id XXX, :title "Fun", :parents []}
     {:id XXX, :title "Other", :parents []}]

(def myaccounts (into [] (ps/accounts mykey myuser :convert? true :minify? true)))
;=> you get the idea
```

From there, you have access to transactions by using:
- `user-transactions`
- `account-transactions`
- `category-transactions`

The same boolean options can be provided to each function
- `normalize?` -> reduces the category and account associated with each transaction to it's ID
- `convert?` -> converts datetimes, money, currency, and timezones to an appropriate joda object
- `minify?` -> returns a map with the bare minimum keys

The transaction functions also take query-parameters. `transaction-query-params` helps you construct a reasonable query.
```clojure
-------------------------
crinklywrappr.pocketsmith-api.core/transaction-query-params
([{:keys [start-date end-date updated-since search uncategorized? type needs-review? per-page], :or {per-page 100}}])
  formats a request map for the transaction functions.

  start_date, end_date => local date objects, inclusive
  updated_since => datetime object, with time-zone (use to-time-zone, or from-time-zone)
  type => :debit or :credit
  uncategorized?, needs_review? => boolean
  search => search string
  per_page => number between 10 & 100
```

For example:

```clojure
(into [] (ps/category-transactions
          mykey myuser
          (ps/by-name-or-title "Interest" mycategories)
          (ps/transaction-query-params (ps/last-month myuser))
          :normalize? true :convert? true :minify? true))
```


# Contributions

Check out `build.clj` for functions that can be ran with `clojure -T:build <function>`.

# Limitations

## Unsupported currencies

`clojurewerkz/money` supports most of the (currently) 175 currencies pocketsmith knows about. If you need support for one of the following currencies, then you will need to roll your own.

- BYN
- CLF
- CUC
- EEK
- GGP
- IMP
- JEP
- MRU
- MTL
- SSP
- STN
- SVC
- VES
- ZMK

## License

Copyright © 2023 Crinklywrappr

Distributed under the Eclipse Public License version 1.0.
