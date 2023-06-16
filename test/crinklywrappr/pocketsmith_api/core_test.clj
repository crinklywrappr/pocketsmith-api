(ns crinklywrappr.pocketsmith-api.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.reducers :as r]
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

(defn with-time-zones [f]
  (with-redefs [ps/time-zones* (fn [_] (r/map identity time-zones))]
    (f)))

(use-fixtures :once with-time-zones)

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
                         :key "key" :opts {:query-params {:per_page 100}}}}]
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
              (let [code (ma/currency-of money)
                    amount (psgen/money->bigdec money)]
                (ps/amount->money amount code)))
            monies)))))

(defspec usd-test 100
  (prop/for-all [money (psgen/money* mc/USD {})]
    (let [code (ma/currency-of money)
          amount (psgen/money->bigdec money)]
      (is (= money (ps/amount->money amount code))))))

(defspec jpy-test 100
  (prop/for-all [money (psgen/money* mc/JPY {})]
    (let [code (ma/currency-of money)
          amount (psgen/money->bigdec money)]
      (is (= money (ps/amount->money amount code))))))

(deftest amount->money-exception-test
  (is (thrown? ClassCastException (ps/amount->money 10M "usd"))))

(defspec account-test 50
  (prop/for-all [user (gen/fmap ps/convert-currency-code-on-user psgen/user)]
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
  (testing "assertion error"
    (is (thrown? AssertionError (ps/accounts "key" {:id 1} :convert? true))))
  (let [user (gen/generate (gen/fmap (comp #(assoc % :id 1) ps/convert-currency-code-on-user) psgen/user))]
    (testing "zero accounts"
      (with-redefs [client/get (mock-response [])]
        (is (= [{:status 200
                 :body nil
                 :parse-error "class java.lang.NullPointerException"
                 :headers nil
                 :request {:uri "https://api.pocketsmith.com/v2/users/1/transaction_accounts"
                           :key "key" :opts {:query-params {:per_page 100}}}}]
               (into [] (ps/accounts "key" user))
               (into [] (ps/accounts "key" user :convert? true :minify? true))))))
    (testing "problem fetching accounts"
      (let [user (gen/generate (gen/fmap ps/convert-currency-code-on-user psgen/user))
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
          (is (every? #(contains? % :id) (butlast (into [] (ps/accounts "key" user :convert? true :minify? true))))))))))

(defspec transaction-test 10
  (prop/for-all [user (gen/fmap ps/convert-currency-code-on-user psgen/user)]
    (let [transactions (gen/generate (gen/vector (psgen/transaction user) 1 100))]
      (with-redefs [client/get (mock-response (partition-all 10 transactions))]
        (is (= (mapv :id transactions)
               (mapv :id (into [] (ps/user-transactions "key" user {})))
               (mapv :id (into [] (ps/user-transactions "key" user {} :convert? true)))
               (mapv :id (into [] (ps/user-transactions "key" user {} :minify? true)))
               (mapv :id (into [] (ps/user-transactions "key" user {} :normalize? true)))
               (mapv :id (into [] (ps/user-transactions "key" user {} :convert? true :minify? true)))
               (mapv :id (into [] (ps/user-transactions "key" user {} :minify? true :normalize? true)))
               (mapv :id (into [] (ps/user-transactions "key" user {} :convert? true :normalize? true)))
               (mapv :id (into [] (ps/user-transactions "key" user {} :convert? true :minify? true :normalize? true)))

               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {})))
               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true)))
               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {} :minify? true)))
               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {} :normalize? true)))
               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true :minify? true)))
               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {} :minify? true :normalize? true)))
               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true :normalize? true)))
               (mapv :id (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true)))

               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {})))
               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true)))
               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {} :minify? true)))
               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {} :normalize? true)))
               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true :minify? true)))
               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {} :minify? true :normalize? true)))
               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true :normalize? true)))
               (mapv :id (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true)))))))))

(deftest transactions-problem-test
  (testing "assertion error"
    (is (thrown? AssertionError (ps/user-transactions "key" {:id 1} {} :convert? true)))
    (is (thrown? AssertionError (ps/account-transactions "key" {:id 1} {:id 1} {} :convert? true)))
    (is (thrown? AssertionError (ps/category-transactions "key" {:id 1} {:id 1} {} :convert? true))))
  (let [user (gen/generate (gen/fmap (comp #(assoc % :id 1) ps/convert-currency-code-on-user) psgen/user))]
    (testing "zero transactions"
      (with-redefs [client/get (mock-response [])]
        (is (= [{:status 200
                 :body nil
                 :parse-error "class java.lang.NullPointerException"
                 :headers nil
                 :request {:uri "https://api.pocketsmith.com/v2/users/1/transactions"
                           :key "key" :opts {:query-params {:per_page 100}}}}]
               (into [] (ps/user-transactions "key" user {}))
               (into [] (ps/user-transactions "key" user {} :convert? true :minify? true :normalize? true))))
        (is (= [{:status 200
                 :body nil
                 :parse-error "class java.lang.NullPointerException"
                 :headers nil
                 :request {:uri "https://api.pocketsmith.com/v2/transaction_accounts/1/transactions"
                           :key "key" :opts {:query-params {:per_page 100}}}}]
               (into [] (ps/account-transactions "key" user {:id 1} {}))
               (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true))))
        (is (= [{:status 200
                 :body nil
                 :parse-error "class java.lang.NullPointerException"
                 :headers nil
                 :request {:uri "https://api.pocketsmith.com/v2/categories/1/transactions"
                           :key "key" :opts {:query-params {:per_page 100}}}}]
               (into [] (ps/category-transactions "key" user {:id 1} {}))
               (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true))))))
    (testing "problem fetching transactions"
      (let [transactions (partition-all 3 (gen/generate (gen/vector (psgen/transaction user) 9)))]
        (with-redefs [client/get (mock-error-response transactions 0)]
          ;; user
          (is (== 1 (count (into [] (ps/user-transactions "key" user {})))))
          (is (ps/error-response? (first (into [] (ps/user-transactions "key" user {})))))
          (is (= (into [] (ps/user-transactions "key" user {}))
                 (into [] (ps/user-transactions "key" user {} :convert? true :minify? true :normalize? true))))
          ;; account
          (is (== 1 (count (into [] (ps/account-transactions "key" user {:id 1} {})))))
          (is (ps/error-response? (first (into [] (ps/account-transactions "key" user {:id 1} {})))))
          (is (= (into [] (ps/account-transactions "key" user {:id 1} {}))
                 (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true))))
          ;; category
          (is (== 1 (count (into [] (ps/category-transactions "key" user {:id 1} {})))))
          (is (ps/error-response? (first (into [] (ps/category-transactions "key" user {:id 1} {})))))
          (is (= (into [] (ps/category-transactions "key" user {:id 1} {}))
                 (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true)))))
        (with-redefs [client/get (mock-error-response transactions 1)]
          ;; user
          (is (> (count (into [] (ps/user-transactions "key" user {}))) 1))
          (is (ps/error-response? (last (into [] (ps/user-transactions "key" user {})))))
          (is (= (last (into [] (ps/user-transactions "key" user {})))
                 (last (into [] (ps/user-transactions "key" user {} :convert? true :minify? true :normalize? true)))))
          (is (every? #(contains? % :id) (butlast (into [] (ps/user-transactions "key" user {})))))
          (is (every? #(contains? % :id) (butlast (into [] (ps/user-transactions "key" user {} :convert? true :minify? true :normalize? true)))))
          ;; account
          (is (> (count (into [] (ps/account-transactions "key" user {:id 1} {}))) 1))
          (is (ps/error-response? (last (into [] (ps/account-transactions "key" user {:id 1} {})))))
          (is (= (last (into [] (ps/account-transactions "key" user {:id 1} {})))
                 (last (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true)))))
          (is (every? #(contains? % :id) (butlast (into [] (ps/account-transactions "key" user {:id 1} {})))))
          (is (every? #(contains? % :id) (butlast (into [] (ps/account-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true)))))
          ;; category
          (is (> (count (into [] (ps/category-transactions "key" user {:id 1} {}))) 1))
          (is (ps/error-response? (last (into [] (ps/category-transactions "key" user {:id 1} {})))))
          (is (= (last (into [] (ps/category-transactions "key" user {:id 1} {})))
                 (last (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true)))))
          (is (every? #(contains? % :id) (butlast (into [] (ps/category-transactions "key" user {:id 1} {})))))
          (is (every? #(contains? % :id) (butlast (into [] (ps/category-transactions "key" user {:id 1} {} :convert? true :minify? true :normalize? true))))))))))
