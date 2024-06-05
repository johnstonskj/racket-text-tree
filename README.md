Racket Package text-tree
=========
This package provides a simple interface to output tree-structured data.

[![raco pkg install text-tree](https://img.shields.io/badge/raco%20pkg%20install-text--tree-blue.svg)](http://pkgs.racket-lang.org/package/text-tree)
[![Documentation](https://img.shields.io/badge/raco%20docs-text--tree-blue.svg)](http://docs.racket-lang.org/text-tree/index.html)
[![Racket](https://github.com/johnstonskj/racket-text-tree/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-text-tree/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-text-tree.svg?style=flat-square)](https://github.com/johnstonskj/racket-text-tree/releases)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-text-tree.svg)](https://github.com/johnstonskj/racket-text-tree/stargazers)

## Example

TBD

```racket
(displayln
  (with-output-to-string
    (λ () (write-text-tree
            (make-hash
              '((collection . "text-tree")
                (deps "base")
                (build-deps "scribble-lib" "racket-doc" "rackunit-lib")
                (scribblings "scribblings/text-tree.scrbl")
                (pkg-desc . "Simple interface to output tree-structured data.")
                (version . "0.1.0")
                (pkg-authors johnstonskj)
                (license . Apache-2.0)))))))
```

Results in the following:

```text
├── build-deps
│   ├── scribble-lib
│   ├── racket-doc
│   └── rackunit-lib
├── collection
│   └── text-tree
├── deps
│   └── base
├── license
│   └── Apache-2.0
├── pkg-authors
│   └── johnstonskj
├── pkg-desc
│   └── Simple interface to output tree-structured data.
├── scribblings
│   └── scribblings/text-tree.scrbl
└── version
    └── 0.1.0
```

## Changes

**Version 1.1**

This release fixes all the known issues with nested structure handling.

* Feature: it also expands the allowed types by using the `sequence` and `dictionary` generics to
  abstract over various concrete types.
* Feature: add a parameter for the name of empty sequences and dictionaries.
* Tests: test cases expanded, more required.
* Docs: all docs completed, more examples would be good.

**Version 1.0**

Initial release.

This release has known defects in nested structure handling.
