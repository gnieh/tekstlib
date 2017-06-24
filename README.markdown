Gnieh Tekstlib [![Build Status](https://travis-ci.org/gnieh/tekstlib.png)](https://travis-ci.org/gnieh/tekstlib)
==============

Gnieh Tekstlib is a library grouping standard classes and packages allowing people to work with texts and documents.

Tekstlib is published to sonatype and central maven repositories and is built for Scala 2.11 and 2.12.

Here is what you want to use in your SBT file

```scala
libraryDependencies += "org.gnieh" %% "tekstlib" % "0.1.1"
```

Package Structure
-----------------

 - `gnieh.diff` diff manipulation,
 - `gnieh.matching` string matching algorithms (exact or fuzzy),
 - `gnieh.hyphen` hyphenation for text in any language,
 - `gnieh.string` strings manipulation for text processing,
 - `gnieh.mustache` lightweight mustache template processor,
 - `gnieh.pp` pretty printing related classes,
 - `gnieh.regex` regular expressions manipulation.

