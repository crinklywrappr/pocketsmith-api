# com.github.crinklywrappr/pocketsmith-api

Small library for interacting with [Pocketsmith](https://www.pocketsmith.com/) using their REST API.

## Usage

FIXME: write usage documentation!

Invoke a library API function from the command-line:

    $ clojure -X crinklywrappr.pocketsmith-api/foo :a 1 :b '"two"'
    {:a 1, :b "two"} "Hello, World!"

Run the project's tests (they'll fail until you edit them):

    $ clojure -T:build test

Run the project's CI pipeline and build a JAR (this will fail until you edit the tests to pass):

    $ clojure -T:build ci

This will produce an updated `pom.xml` file with synchronized dependencies inside the `META-INF`
directory inside `target/classes` and the JAR in `target`. You can update the version (and SCM tag)
information in generated `pom.xml` by updating `build.clj`.

Install it locally (requires the `ci` task be run first):

    $ clojure -T:build install

Deploy it to Clojars -- needs `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` environment
variables (requires the `ci` task be run first):

    $ clojure -T:build deploy

Your library will be deployed to com.github.crinklywrappr/pocketsmith-api on clojars.org by default.

# Limitations

## Unsupported user time-zones

Pocketsmith reports user time-zones using a format that `clj-time` doesn't understand. For example: `Central Time (US & Canada)` instead of `America/Chicago`. Until that changes, or a workaround is added, `pocketsmith-api` will be unable to report date-times in the user's time-zone.

## Unsupported currencies

`clojurewerkz/money` supports most of the (currently) 175 currencies pocketsmith knows about. If you need support for one of the following currencies, then you will need to roll your own.

- "BYN"
- "CLF"
- "CUC"
- "EEK"
- "GGP"
- "IMP"
- "JEP"
- "MRU"
- "MTL"
- "SSP"
- "STN"
- "SVC"
- "VES"
- "ZMK"

## License

Copyright © 2023 Crinklywrappr

Distributed under the Eclipse Public License version 1.0.
