# graze

A web crawler, using a pool of lightweight threads for concurrent crawling. It
features a parser for `robots.txt` files and a specialized data structure that
allows efficient querying of the resulting rules.

## Installation

Follow the usual [stack](https://www.haskellstack.org) workflow.

## Usage

**Example:**

```
$ stack run -- https://www.iana.org records --depth=1
Crawling https://www.iana.org
Got https://www.iana.org
Got https://www.iana.org/about
Got https://www.iana.org/domains/root
...
Got https://www.iana.org/reports
Got https://www.iana.org/domains/idn-tables
Done
```

**Help page:**

```
$ stack run -- --help
Usage: graze-exe <base> <folder> [--depth <depth>] [--threads <threads>]

Available options:
  -h,--help                Show this help text
  <base>                   URL to start at
  <folder>                 Record folder
  --depth <depth>          Depth of the search (default: 3)
  --threads <threads>      Size of the thread pool (default: 10)
```

## Storage model

The example above produces the following file tree:

```
records
├── 13f601567450c419c77c2771594753b6dc2add5e.json
├── 21238e043428d8a778463fb7ce0dc54370b1a028.json
...
└── fec01f0f176e22939d47435cc1d5a63b02030d9b.json
```

For every visited URL the crawler creates a JSON-encoded record that represents
the corresponding node in the crawl tree.

The record's name is derived from the URL's SHA-1 digest; for example,

```
https://www.iana.org/protocols/apply
```

becomes

```
13f601567450c419c77c2771594753b6dc2add5e.json
```
