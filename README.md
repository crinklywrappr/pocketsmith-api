# com.github.crinklywrappr/pocketsmith-api

Small library for interacting with [Pocketsmith](https://www.pocketsmith.com/) using their REST API.

# Usage



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
