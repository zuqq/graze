# graze

Graze is a concurrent web crawler leveraging GHC's lightweight threads,
with an attoparsec-based parser for `robots.txt` files and a trie implementation
for efficiently applying the rules. Please note that it does not employ any kind
of rate limiting.


## Installation

Follow the usual [stack](https://www.haskellstack.org) workflow.


## Usage

**Example:**

```
$ stack run -- https://www.iana.org download --depth=1 --threads=4
Crawling https://www.iana.org/
Got https://www.iana.org/
Got https://www.iana.org/domains
Got https://www.iana.org/protocols/apply
...
Got https://www.iana.org/reports
Got https://www.iana.org/domains/idn-tables
Done
```

**Help page:**

```
Usage: graze-exe URL FOLDER [--depth d] [--threads n]

Available options:
  URL                      URL to start at
  FOLDER                   Download folder
  --depth d                Depth of the search (default: 3)
  --threads n              Number of threads (default: 10)
  -h,--help                Show this help text
```

## Storage model

The example above creates the following file tree:

```
download
├── 13f601567450c419c77c2771594753b6dc2add5e
├── 21238e043428d8a778463fb7ce0dc54370b1a028
...
├── fec01f0f176e22939d47435cc1d5a63b02030d9b
└── records
    ├── 13f601567450c419c77c2771594753b6dc2add5e.json
    ├── 21238e043428d8a778463fb7ce0dc54370b1a028.json
    ...
    └── fec01f0f176e22939d47435cc1d5a63b02030d9b.json
```

For every visited URL we store two things: a file containing the response body
and a JSON-encoded record that contains information about the corresponding node
in the crawl graph. The file names derive from the SHA-1 digest of the URL; for
example, `13f601567450c419c77c2771594753b6dc2add5e` corresponds to
`https://www.iana.org/protocols/apply`.
