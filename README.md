# Optics in the abstract

This repository contains materials to accompany the [Well-Typed](https://well-typed.com/)
workshop at [ZuriHac 2021](https://zfoh.ch/zurihac2021/) on Optics.

 * Workshop will be streamed on YouTube at 14:00 CEST (13:00 BST, 08:00 ET) on
   Saturday 19th June

 * Discussion in the `#optics` channel on the ZuriHac Discord

 * [Optics on Hackage](https://hackage.haskell.org/package/optics)

 * You may wish to install `optics-0.4` so you can experiment in GHCi (any of
   GHC 8.2 or later should work). The `exercises/` directory contains a `.cabal`
   package that can be loaded with `cabal repl`.


## Abstract

Haskell programming often involves manipulating data structures.  Optics are a
compositional abstraction representing "notions of substructure" as first-class
values.  For example, lenses capture the notion of one data structure containing
another, and there are more general optics for read-only, write-only, optional
or repeated substructures.  Judicious use of optics can lead to succinct,
expressive code, yet optics also have a reputation of being impenetrable.

In this workshop, we will explore optics-the-concept in general and the `optics`
library in particular.  This library provides interfaces for defining, combining
and using various kinds of optics.  Crucially, these interfaces are *abstract*,
so they can be understood without reference to the underlying implementation.

We will also discuss how `optics` can help is in addressing the limitations of
Haskell's record system, in conjunction with language extensions such as
`DuplicateRecordFields`, `OverloadedLabels` and the forthcoming
`RecordDotSyntax` and `NoFieldSelectors`.

Finally, we will use `optics` as a case study to reflect on library design in
Haskell.  How can we build libraries that capture powerful concepts and yet
remain easy to use by Haskellers with a wide range of skill levels?

This workshop will be suitable for Haskell programmers with a knowledge of basic
Haskell concepts (polymorphic functions, data types, type classes).  It does not
require familiarity with advanced type system features such as higher-rank types
or type families.


## Further reading

 * [Optics Haddocks](https://hackage.haskell.org/package/optics-0.4/docs/Optics.html)

 * Profunctor Optics: Modular Data Accessors by Matthew Pickering, Jeremy
   Gibbons, Nicolas Wu (The Art, Science, and Engineering of Programming, 2017).

 * Applicative programming with effects by Conor McBride and Ross Patterson
   (JFP, 2008). The paper that introduced Applicative and Traversable.

 * The essence of the iterator pattern by Jeremy Gibbons and Bruno
   C. d. S. Oliveira (JFP, 2009). Relates traversals to iterators, and discusses
   the laws for Traversable.

 * [Free Monoids in Haskell](http://comonad.com/reader/2015/free-monoids-in-haskell/)
   by Dan Doel.  Explains why `Fold s a` is more general than `s -> [a]` once
   infinite structures get involved.

 * [Lens over tea](https://artyom.me/lens-over-tea-1) by Artyom Kazak.  A blog
   post series exploring the implementation of `lens`.

 * [Glassery](http://oleg.fi/gists/posts/2017-04-18-glassery.html) by Oleg
   Grenrus.  A detailed reference to (profunctor) optics.

* A [reddit comment from Edward Kmett](https://www.reddit.com/r/haskell/comments/1o1z8x/simon_peyton_jones_on_lenses_at_the_haskell/ccoe67d/)
   on the history of ideas in `lens`.

 * [Optics by Example](https://leanpub.com/optics-by-example) by Chris Penner.
   A book giving a detailed introduction to `lens`.
