# mchain-haskell
Non-/Persistent Marchov Chain generator based on 4chan boards

* `lib/`    - text-src
* `mc-db/`  - db executable
* `mc-map/` - same program with a map
* `json/`   - JSON board to .txt parser


## How to fetch threads from a 4chan board


* `./gchan -board <a board from 4chan with no less than 10 pages> -to <txt-file to save to>`

#### Example

* `./gchan  -board g -to file.txt`

## How to use the persistent generator

* `./mchain -db <path to .sqlite3-db> -learn <filepath(s) to .txt to learn from>`
* `./mchain -db <path to .sqlite3-db> -get <thunks as a number>`

#### Example

* `./mchain -db g_4chan.sqlite3 -learn file1.txt file2.txt`
* `./mchain -db g_4chan.sqlite3 -get 5`

## How to use the non-persistent generator

* `./mc-map -get <thunks as a number> -from <filepath(s) to .txt to learn from>`

#### Example

* `./mc-map -get 4 -from ../lib/a_01_06_2015.txt ../lib/g_02_06_2015.txt`

#### TODO

* check if any board has less than 10 pages
* allow to fetch archives (currently getting a timeout)
* allow to set thunk-size