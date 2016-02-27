# purescript-lists

[![Latest release](http://img.shields.io/bower/v/purescript-lists.svg)](https://github.com/purescript/purescript-lists/releases)
[![Build Status](https://travis-ci.org/purescript/purescript-lists.svg?branch=master)](https://travis-ci.org/purescript/purescript-lists)
[![Dependency Status](https://www.versioneye.com/user/projects/55848c8a363861001d00033b/badge.svg?style=flat)](https://www.versioneye.com/user/projects/55848c8a363861001d00033b)

This library defines strict and lazy linked lists, and associated helper functions and type class instances.

_Note_: This module is an improvement over `Data.Array` when working with immutable lists of data in a purely-functional setting, but does not have good random-access performance.

## Installation

```
bower install purescript-lists
```

## Licensing

Some of this code is derived from GHC's standard libraries (`base`);
according to its terms, we have included GHC's license in the file
`LICENSE-GHC.md`.

## Module documentation

- [Data.List](docs/Data/List.md)
- [Data.List.Lazy](docs/Data/List/Lazy.md)
- [Data.List.Unsafe](docs/Data/List/Unsafe.md)
- [Data.List.ZipList](docs/Data/List/ZipList.md)
