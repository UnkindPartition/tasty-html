tasty-html
==========

HTML test reporter for the Tasty test framework.


## Example

Here's how your `test.hs` might look like:

```haskell
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html

import Data.List
import Data.Ord

main = defaultMainWithIngredients (htmlRunner:deafultIngredients) tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]
```

And here is the output of the above program rendered to HTML:

![](https://raw.github.com/feuerbach/tasty-html/master/screenshot.png)

(Note that whether QuickCheck finds a counterexample to the third property is
determined by chance.)

## Hacking

When cloning this repository use `--recursive` parameter to checkout the git
submodule pointing to the `bootstrap` fork being used by `tasty-html`.

```
$ git clone --recursive https://github.com/feuerbach/tasty-html
```

Making changes to the `bootstrap` fork is the same procedure followed by the
[upstream project](https://github.com/twbs/bootstrap).

```
$ cd data/bootstrap
$ npm install
```

You might change the style by editing the `less` files. Once you are done, use
`grunt` to compile the `css` files:

```
$ grunt dist
```

This assumes you have `grunt-cli` installed globally, either with `npm` (`npm
install -g grunt-cli`) or from a package manager if available.

Consider submitting your changes as pull requests to the `tasty-html` bootstrap
fork at https://github.com/jdnavarro/bootstrap.
