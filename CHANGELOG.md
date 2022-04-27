# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v7.0.0](https://github.com/purescript/purescript-lists/releases/tag/v7.0.0) - 2022-04-27

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#203 by @JordanMartinez)
- Drop deprecated `MonadZero` instance (#205 by @JordanMartinez)
- Drop deprecated `group'` and `mapWithIndex` (#206 by @JordanMartinez)
- Change `groupAllBy` to use a comparison function (#191)

New features:

Bugfixes:

Other improvements:

## [v6.1.0](https://github.com/purescript/purescript-lists/releases/tag/v6.1.0) - 2022-02-22

Breaking changes:

New features:
- Added `cons` for `Lazy.NonEmptyList` (#143 by @matthewleon)

Bugfixes:

Other improvements:
- Fix ad-hoc usage of case expression (#202 by @JordanMartinez)

## [v6.0.1](https://github.com/purescript/purescript-lists/releases/tag/v6.0.1) - 2021-04-19

Other improvements:
- Fixed warnings revealed by v0.14.1 PS release (#198 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript/purescript-lists/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Converted `nub`/`nubBy` to use ordering, rather than equality (#179)
- Renamed `scanrLazy` to `scanlLazy` and fixed parameter ordering (#161)
- Renamed `group'` to `groupAll` (#182)
- Changed `Alt ZipList` to satisfy distributivity (#150)
- Updated the `Show` instances for (non empty) lazy lists (#181)

New features:
- Added `nubEq`/`nubByEq` (#179)
- Added `groupAllBy` (#182)
- Added `Eq1` and `Ord1` instances to `NonEmptyList` and `LazyNonEmptyList` (#188)

Bugfixes:

Other improvements:
- Fixed Lazy List docs where original list is returned instead of Nothing (#169)
- Migrated to GitHub Actions (#177)
- Changed `foldM` type signature to more closely match `foldl` (#165)
- Improved `foldr` performance on large lists (#180)
- Generated changelog and add PR template (#187) 

## [v5.4.1](https://github.com/purescript/purescript-lists/releases/tag/v5.4.1) - 2019-05-06

Fixes a shadowed name warning in advance of new `purs` version

## [v5.4.0](https://github.com/purescript/purescript-lists/releases/tag/v5.4.0) - 2019-02-09

Added `scanrLazy` for lazy lists (@drewolson)

## [v5.3.0](https://github.com/purescript/purescript-lists/releases/tag/v5.3.0) - 2018-08-31

- Added `cons'` and `snoc'` variations for `NonEmptyList` that construct from an `a`+`List a`

## [v5.2.0](https://github.com/purescript/purescript-lists/releases/tag/v5.2.0) - 2018-08-19

- Added `Unfoldable1` instances for `NonEmptyList`s

## [v5.1.0](https://github.com/purescript/purescript-lists/releases/tag/v5.1.0) - 2018-08-19

- Added `FunctorWithIndex` and related instances for `NonEmptyList`s (@cryogenian)

## [v5.0.0](https://github.com/purescript/purescript-lists/releases/tag/v5.0.0) - 2018-05-23

- Updated for PureScript 0.12
- Fixed a bug in `take` for lazy lists (@ibara1454)
- Added `tails` (@colehaus)

## [v4.12.0](https://github.com/purescript/purescript-lists/releases/tag/v4.12.0) - 2017-11-30

Add indexed `Functor`, `Foldable` and `Traversable` instances (@matthewleon)

## [v4.11.0](https://github.com/purescript/purescript-lists/releases/tag/v4.11.0) - 2017-11-02

Short circuit `drop` and `take` on negative inputs (@mlms13)

## [v4.10.0](https://github.com/purescript/purescript-lists/releases/tag/v4.10.0) - 2017-09-09

Add `dropEnd` and `takeEnd` (@notgiorgi)

## [v4.9.1](https://github.com/purescript/purescript-lists/releases/tag/v4.9.1) - 2017-09-03

Require `foldable-traversable` explicitly (@MonoidMusician)

## [v4.9.0](https://github.com/purescript/purescript-lists/releases/tag/v4.9.0) - 2017-07-06

Add `stripPrefix` for `List.Lazy` (@safareli)

## [v4.8.0](https://github.com/purescript/purescript-lists/releases/tag/v4.8.0) - 2017-06-30

- Added most list operations for `NonEmptyList`
- Added `Foldable1` and `Traversable1` instances for `NonEmptyList`

## [v4.7.0](https://github.com/purescript/purescript-lists/releases/tag/v4.7.0) - 2017-06-13

- Added `cons`, `snoc`, and `unsnoc` for `Data.List.NonEmpty` (@natefaubion)

## [v4.6.1](https://github.com/purescript/purescript-lists/releases/tag/v4.6.1) - 2017-06-07

Require `tailrec^3.3.0`

## [v4.6.0](https://github.com/purescript/purescript-lists/releases/tag/v4.6.0) - 2017-06-04

Add `stripPrefix` and `Pattern` to `Data.List` (@safareli)

## [v4.5.0](https://github.com/purescript/purescript-lists/releases/tag/v4.5.0) - 2017-06-04

Add `foldrLazy` (@matthewleon)

## [v4.4.0](https://github.com/purescript/purescript-lists/releases/tag/v4.4.0) - 2017-06-03

Add `partition` (@matthewleon)

## [v4.3.0](https://github.com/purescript/purescript-lists/releases/tag/v4.3.0) - 2017-05-28

- `sort` and `sortBy` for `NonEmptyList` (@matthewleon)

## [v4.2.0](https://github.com/purescript/purescript-lists/releases/tag/v4.2.0) - 2017-05-28

- Reimplement `snoc` using `foldr` (@LiamGoodacre)
- Use `~>` where appropriate (@matthewleon)
- Add `repeat` and `iterate` for non-empty lazy lists (@matthewleon)

## [v4.1.1](https://github.com/purescript/purescript-lists/releases/tag/v4.1.1) - 2017-05-09

- Export `last` for `NonEmptyList`s

## [v4.1.0](https://github.com/purescript/purescript-lists/releases/tag/v4.1.0) - 2017-04-27

- Added `some` and `many` for `Data.List.Lazy` (@safareli)

## [v4.0.1](https://github.com/purescript/purescript-lists/releases/tag/v4.0.1) - 2017-03-29

- Restore compiler-optimized TCO operations

## [v4.0.0](https://github.com/purescript/purescript-lists/releases/tag/v4.0.0) - 2017-03-26

- Updated for PureScript 0.11.0
- Swapped arguments of `mapWithIndex` for consistency with `purescript-arrays`
- Various code style tweaks & warning fixes (@matthewleon)
- Added `appendFoldable` for `NonEmptyList`s (@matthewleon)

## [v3.4.0](https://github.com/purescript/purescript-lists/releases/tag/v3.4.0) - 2017-01-29

Add `unsnoc` (@joshuahhh)

## [v3.3.0](https://github.com/purescript/purescript-lists/releases/tag/v3.3.0) - 2016-12-27

Use `~>` where appropriate (@mlang)

## [v3.2.2](https://github.com/purescript/purescript-lists/releases/tag/v3.2.2) - 2016-12-24

Stack safety for various functions (@sammthomson)

## [v3.2.1](https://github.com/purescript/purescript-lists/releases/tag/v3.2.1) - 2016-11-14

- Fixed shadowed name warnings

## [v3.2.0](https://github.com/purescript/purescript-lists/releases/tag/v3.2.0) - 2016-11-02

- Adds an implementation of `traverse` which does not grow the stack.

## [v3.1.0](https://github.com/purescript/purescript-lists/releases/tag/v3.1.0) - 2016-10-28

- Added `manyRec` and `someRec` - stack safe versions of `many` and `some` via `MonadRec`.

## [v3.0.1](https://github.com/purescript/purescript-lists/releases/tag/v3.0.1) - 2016-10-28

- Fix `purescript-generics` dependency to use `^` rather than `v`.

## [v3.0.0](https://github.com/purescript/purescript-lists/releases/tag/v3.0.0) - 2016-10-16

- The `group` functions now return `NonEmptyList` typed values rather than `NonEmpty List`
- The lazy non-empty list is now lazier
- The modules have been rearranged internally

## [v2.1.0](https://github.com/purescript/purescript-lists/releases/tag/v2.1.0) - 2016-10-13

- Added `Data.List.NonEmpty` (a `newtype` of `NonEmpty List a` with additional instances)
- Added `Extend` instance for `List`

## [v2.0.0](https://github.com/purescript/purescript-lists/releases/tag/v2.0.0) - 2016-10-09

- Updated dependencies
- The `group` functions now return `NonEmpty` groups
- `show` now uses `:` in printing

## [v1.0.1](https://github.com/purescript/purescript-lists/releases/tag/v1.0.1) - 2016-06-11

- Added `Fail` instance for `Bind ZipList` (@joneshf)

## [v1.0.0](https://github.com/purescript/purescript-lists/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v1.0.0-rc.3](https://github.com/purescript/purescript-lists/releases/tag/v1.0.0-rc.3) - 2016-05-22

- Added `Generic` instance for `List`

## [v1.0.0-rc.2](https://github.com/purescript/purescript-lists/releases/tag/v1.0.0-rc.2) - 2016-05-20

- Fixed errors and warnings

## [v1.0.0-rc.1](https://github.com/purescript/purescript-lists/releases/tag/v1.0.0-rc.1) - 2016-03-25

- Release candidate for the psc 0.8+ core libraries

## [v0.7.10](https://github.com/purescript/purescript-lists/releases/tag/v0.7.10) - 2016-02-27

Add `transpose` (@hdgarrood)

## [v0.7.9](https://github.com/purescript/purescript-lists/releases/tag/v0.7.9) - 2016-01-11

Add `fromFoldable`, `toUnfoldable` (@hdgarrood)

## [v0.7.8](https://github.com/purescript/purescript-lists/releases/tag/v0.7.8) - 2016-01-06

Make list `unfoldr` stack-safe (@rgrempel)

## [v0.7.7](https://github.com/purescript/purescript-lists/releases/tag/v0.7.7) - 2015-11-02

- Removed unused imports

## [v0.7.6](https://github.com/purescript/purescript-lists/releases/tag/v0.7.6) - 2015-10-16

- Fixed warnings

## [v0.7.5](https://github.com/purescript/purescript-lists/releases/tag/v0.7.5) - 2015-09-27

Fix imports.

## [v0.7.4](https://github.com/purescript/purescript-lists/releases/tag/v0.7.4) - 2015-09-08

Optimizations (@cryogenian)

## [v0.7.3](https://github.com/purescript/purescript-lists/releases/tag/v0.7.3) - 2015-08-31

Make `foldl` stack-safe.

## [v0.7.2](https://github.com/purescript/purescript-lists/releases/tag/v0.7.2) - 2015-08-16

Update `LICENSE` file to include GHC license.

## [v0.7.1](https://github.com/purescript/purescript-lists/releases/tag/v0.7.1) - 2015-08-03

- Fixed warning messages about shadowed names and partial pattern matches

## [v0.7.0](https://github.com/purescript/purescript-lists/releases/tag/v0.7.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.7.0-rc.5](https://github.com/purescript/purescript-lists/releases/tag/v0.7.0-rc.5) - 2015-06-16

Add `ZipList`.

## [v0.7.0-rc.4](https://github.com/purescript/purescript-lists/releases/tag/v0.7.0-rc.4) - 2015-06-15

Fix `updateAt`, `alterAt`.

## [v0.7.0-rc.3](https://github.com/purescript/purescript-lists/releases/tag/v0.7.0-rc.3) - 2015-06-14

- Fixed behaviour of `range` when `start = end` (@sharkdp)

## [v0.7.0-rc.2](https://github.com/purescript/purescript-lists/releases/tag/v0.7.0-rc.2) - 2015-06-11

Fix `test-src`

## [v0.7.0-rc.1](https://github.com/purescript/purescript-lists/releases/tag/v0.7.0-rc.1) - 2015-06-08

Initial release candidate of the library intended for the 0.7 compiler.

## [v0.6.2](https://github.com/purescript/purescript-lists/releases/tag/v0.6.2) - 2015-05-09

Fix typo in `ListT Apply` and add `ZipListT` (@puffnfresh)

## [v0.6.1](https://github.com/purescript/purescript-lists/releases/tag/v0.6.1) - 2015-05-08

Make `Apply` and `Bind` instances for `ListT` consistent. (@puffnfresh)

## [v0.6.0](https://github.com/purescript/purescript-lists/releases/tag/v0.6.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies

## [v0.5.0](https://github.com/purescript/purescript-lists/releases/tag/v0.5.0) - 2015-01-10

- Updated `purescript-transformers`, `purescript-lazy`, and `purescript-quickcheck` dependencies (@garyb)

## [v0.4.0](https://github.com/purescript/purescript-lists/releases/tag/v0.4.0) - 2015-01-10

Remove `scanl`, bump `foldable` dependency.

## [v0.3.9](https://github.com/purescript/purescript-lists/releases/tag/v0.3.9) - 2014-12-13



## [v0.3.8](https://github.com/purescript/purescript-lists/releases/tag/v0.3.8) - 2014-12-09

Bump `unfoldable` dependency.

## [v0.3.7](https://github.com/purescript/purescript-lists/releases/tag/v0.3.7) - 2014-11-26



## [v0.3.5](https://github.com/purescript/purescript-lists/releases/tag/v0.3.5) - 2014-11-18

Fixes for compiler version 0.6.1

## [v0.3.4](https://github.com/purescript/purescript-lists/releases/tag/v0.3.4) - 2014-11-14



## [v0.3.3](https://github.com/purescript/purescript-lists/releases/tag/v0.3.3) - 2014-11-08



## [v0.3.2](https://github.com/purescript/purescript-lists/releases/tag/v0.3.2) - 2014-10-29



## [v0.3.1](https://github.com/purescript/purescript-lists/releases/tag/v0.3.1) - 2014-10-06



## [v0.3.0](https://github.com/purescript/purescript-lists/releases/tag/v0.3.0) - 2014-10-01

- `ListT` by @jdegoes

## [v0.2.0](https://github.com/purescript/purescript-lists/releases/tag/v0.2.0) - 2014-08-13



## [v0.0.1](https://github.com/purescript/purescript-lists/releases/tag/v0.0.1) - 2014-07-07


