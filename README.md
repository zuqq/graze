# graze

Graze is a concurrent web crawler leveraging GHC's lightweight threads. Please
note that, although it respects disallow rules in the target's `robots.txt`,
Graze does not employ any kind of rate limiting.


## Installation

Follow the usual [stack](https://www.haskellstack.org) workflow.


## Usage

```
Usage: graze-exe [--depth d] [--threads n] --folder FOLDER URL

Available options:
  --depth d                Depth of the search
  --threads n              Number of threads
  --folder FOLDER          Download folder
  URL                      URL to start at
  -h,--help                Show this help text
```
