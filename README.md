# clojure-argparse

A command-line argument parsing library for Clojure, inspired by Python's argparse.

## Overview

clojure-argparse provides a simple and flexible way to parse command-line arguments in Clojure applications. It supports both short (`-x`) and long (`--foo`) option formats with an intuitive API that will feel familiar to developers who have used Python's argparse module.

## Features

- Short option parsing (`-x value`)
- Long option parsing (`--foo value`)
- Multiple option strings per argument (`-v`, `--verbose`)
- Custom destination keys for parsed values
- Default value support
- Numeric argument handling (e.g., `-x -1` is parsed correctly)
- Schema validation via Malli

## Requirements

- Clojure 1.12.0 or later

## Installation

Add the following dependency to your `deps.edn`:

```clojure
{:deps
 {io.github.conao3/clojure-argparse {:git/tag "v0.1.0" :git/sha "..."}}}
```

## Usage

### Basic Example

```clojure
(require '[argparse.core :as argparse])

;; Create a parser and add arguments
(def parser
  (-> {}
      (argparse/add-argument "-x")
      (argparse/add-argument "--foo")))

;; Parse command-line arguments
(argparse/parse-args parser ["-x" "value1" "--foo" "value2"])
;; => {:x "value1" :foo "value2"}
```

### Multiple Option Strings

You can specify multiple option strings for a single argument:

```clojure
(def parser
  (-> {}
      (argparse/add-argument ["-v" "--verbose"])))

(argparse/parse-args parser ["-v" "true"])
;; => {:verbose "true"}

(argparse/parse-args parser ["--verbose" "yes"])
;; => {:verbose "yes"}
```

### Default Values

Set default values for arguments:

```clojure
(def parser
  (-> {}
      (argparse/add-argument "-x")
      (argparse/add-argument "-y" :default 42)))

(argparse/parse-args parser [])
;; => {:x nil :y 42}
```

### Custom Destination

Override the destination key used to store the parsed value:

```clojure
(def parser
  (-> {}
      (argparse/add-argument "--baz" :dest :zabbaz)))

(argparse/parse-args parser ["--baz" "value"])
;; => {:zabbaz "value"}
```

### Numeric Arguments

Numeric values starting with a dash are handled correctly:

```clojure
(def parser
  (-> {}
      (argparse/add-argument "-x")))

(argparse/parse-args parser ["-x" "-1"])
;; => {:x "-1"}

(argparse/parse-args parser ["-x" "-2.5"])
;; => {:x "-2.5"}
```

## API Reference

### add-argument

```clojure
(add-argument parser option-string & {:keys [action dest default nargs]})
```

Adds an argument specification to the parser.

**Parameters:**
- `parser` - The parser map
- `option-string` - A string or collection of strings defining the option (e.g., `"-x"` or `["-v" "--verbose"]`)
- `:dest` - Keyword to use as the key in the result map (optional, inferred from option string)
- `:default` - Default value when the option is not provided (optional, defaults to `nil`)
- `:action` - Function to apply to the parsed value (optional)
- `:nargs` - Number of arguments to consume (optional)

### parse-args

```clojure
(parse-args parser args)
```

Parses the given arguments according to the parser specification.

**Parameters:**
- `parser` - The parser map created with `add-argument`
- `args` - A sequence of command-line argument strings

**Returns:** A map of destination keywords to parsed values.

## Development

### Running Tests

```bash
clojure -M:test
```

### Building

```bash
clojure -T:build
```

## License

Copyright (c) Naoya Yamashita

This project is licensed under the terms of the MIT License.
