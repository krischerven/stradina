# Stradina

An interactive research application used to analyze travel data.

## Usage

Binaries are not provided for Stradina, which is intended to be run as an interactive Clojure program.
To run Stradina, you need to install Clojure and an IDE such as Cider (Emacs) or Calva (VSCode).

https://clojure.org/guides/install_clojure

After installing Clojure, simply run the project in your IDE of choice.

``` clojure
;; Add a Google Maps API key
(add-API-key :maps "FIXME_PUT_YOUR_API_KEY_HERE")

;; Print customized directions between two local destinations
(print-directions "Detroit Institute of Art, Detroit, MI" "Max M. & Marjorie S. Fisher Music Center, Detroit, MI")

;; Add a walk data point to local storage (330 meters, 300 seconds)
(add-walk-data-point "Somewhere I went earlier" 330 300)

;; Add a walk data point to local storage (500 meters, 300 seconds)
(add-walk-data-point-with-note "Somewhere I went after that" "NOTE: walked about 50% faster than usual" 500 300)

;; Check our walk speed/time
(my-average-walk-speed)
(my-average-walk-time)

;; Ask a local AI model to interpret our walk data (redacting notes and tags to be safe)
(ollama-interpret-movement-data (redact (walk-data)))
```

## License

Copyright © 2024 Stradina Developers

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
