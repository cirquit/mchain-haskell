# mchain-haskell
Non-/Persistent Marchov Chain generator based on 4chan boards

* `lib/`    - text-source
* `src/`    - .hs-Files
  * `src/mc-map`    - implemented with a map
  * `src/mc-db`     - implemented with a sqlite-db
  * `src/json`      - threadgrepper with json
* `bin/`    - executable


### How to compile

  * `make json` in `/`
  * `make mc-map` in `/`
  * `make mc-db` in `/`

## How to fetch threads from a 4chan board


* `./jchan -board <a board from 4chan with no less than 10 pages> -to <txt-file to save to>`

#### Example

* `./jchan  -board g -to file.txt`

## How to use the persistent generator

* `./mdb -db <path to .sqlite3-db> -learn <filepath(s) to .txt to learn from>`
* `./mdb -db <path to .sqlite3-db> -get <thunks as a number>`

#### Example

* `./mdb -db g_4chan.sqlite3 -learn file1.txt file2.txt`
* `./mdb -db g_4chan.sqlite3 -get 5`

## How to use the non-persistent generator

* `./mmap -get <thunks as a number> -from <filepath(s) to .txt to learn from>`

#### Example

* `./mmap -get 4 -from ../lib/a_01_06_2015.txt ../lib/g_02_06_2015.txt`

#### TODO

* check if any board has less than 10 pages
* allow to fetch archives (currently getting a timeout)
* allow to set thunk-size