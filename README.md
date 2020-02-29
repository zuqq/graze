# graze

Graze is a concurrent web crawler leveraging GHC's lightweight threads. Please
note that, although it respects disallow rules in the target's `robots.txt`,
Graze does not employ any kind of rate limiting.


## Installation

Follow the usual [stack](https://www.haskellstack.org) workflow.


## Usage

```
Usage: graze-exe [--workers n] [--depth d] [--folder f] [--database db] base

Available options:
  --workers n              Number of worker threads
  --depth d                Depth of the search
  --folder f               Folder to save the pages in
  --database db            Name of the resulting CSV
  base                     URL for the crawler to start at
  -h,--help                Show this help text
```
