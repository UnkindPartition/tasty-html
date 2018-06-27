0.4.1.2 - 2018-06-27
--------------------

Fix compatibility with GHC 8.4


0.4.1.1 - 2015-11-10
--------------------

- Report running time for tests.

0.4.1 - 2014-11-18
------------------

- Export `AssetsPath`.

0.4 - 2014-11-11
----------------

- Add option to load bootstrap assets externally.

0.3
---

This release is mainly about upgrading to bootstrap3.

* Modifications to bootstrap are done directly in its source, which is
  now maintained as a fork at https://github.com/jdnavarro/bootstrap. The
  modifications are mainly about inlining resources.
* Icons are now true fonts instead of image sprites.
* Test trees are represented as [*media
  objects*](http://getbootstrap.com/components/#media).
* Remove dependendency to bootstrap-tree.
* Upgrade to `tasty-0.10`.
* Fix HTML malformation.

0.2
---
* Remove forgotten `undefined` that made failing tests crash.
