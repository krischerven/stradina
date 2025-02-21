# Stradina

An interactive research application used to analyze travel data.

## Usage

Binaries are not provided for Stradina, which is intended to be run as an interactive Clojure program.
To run Stradina, you need to install Clojure and an IDE such as Cider (Emacs) or Calva (VSCode).

https://clojure.org/guides/install_clojure

After installing Clojure, simply run the project in your IDE of choice.

``` clojure
(add-API-key :maps "FIXME_PUT_YOUR_API_KEY_HERE")
(print-directions "Detroit Institute of Art, Detroit, MI" "Max M. & Marjorie S. Fisher Music Center, Detroit, MI")
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
