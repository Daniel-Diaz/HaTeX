
# The HaTeX library

HaTeX is a Haskell library that implements the *LaTeX syntax*, plus some abstractions on top.

Check a list of usage examples in the [Examples](https://github.com/Daniel-Diaz/HaTeX/tree/master/Examples) directory
of the repository in GitHub.
A good starting point is [simple.hs](https://github.com/Daniel-Diaz/HaTeX/blob/master/Examples/simple.hs).
Run any example script executing the ``main`` function.

## Installation notes

To install `HaTeX`, use [cabal-install](http://hackage.haskell.org/package/cabal-install)
or [stack](http://docs.haskellstack.org/en/stable/README).

To install with cabal, run:

    $ cabal update
    $ cabal install HaTeX

This will download and install the latest official release (recommended).
If you want to try a newer version, use _git_ to clone the code contained
in this repository.

    $ git clone https://github.com/Daniel-Diaz/HaTeX.git
    $ cd HaTeX
    $ cabal install

However, this is not recommended as it may include some bugs or oddities due to in-development features.
Note that this package follows the [_Package Versioning Policy_](http://www.haskell.org/haskellwiki/Package_versioning_policy),
so it is unlikely to suffer from API breakages if you follow it too when importing the library (assuming
you are using the version in Hackage).

## Current HaTeX versions

[![HaTeX on Hackage](https://img.shields.io/hackage/v/HaTeX.svg)](https://hackage.haskell.org/package/HaTeX)

[![HaTeX on Stackage](http://stackage.org/package/HaTeX/badge/lts)](http://stackage.org/lts/package/HaTeX)

[![HaTeX on Stackage Nightly](http://stackage.org/package/HaTeX/badge/nightly)](http://stackage.org/nightly/package/HaTeX)

## HaTeX User's Guide

The HaTeX User's Guide lives [here](https://github.com/Daniel-Diaz/hatex-guide)... and is also done in Haskell!
It is free source and anybody can contribute to it. Doing so, you will help current and future users!

A downloadable version (not necessarily the latest version, but most likely)
can be found [here](http://daniel-diaz.github.com/projects/hatex/hatex-guide.pdf).
To be sure that you are reading the last version, go to the github repository of the guide and follow instructions
to build it. It is fairly easy.

Please note that the user's guide needs to be updated (contributions are more than welcome!).

## Community and Contributions

There are many ways to get involved in the HaTeX project. Use the most comfortable way for you.

* Fork the [GitHub repository](https://github.com/Daniel-Diaz/HaTeX).
* Report bugs or make suggestions opening a ticket in the [Issue Tracker](https://github.com/Daniel-Diaz/HaTeX/issues).
* Help us to improve and extend our [hatex-guide](https://github.com/Daniel-Diaz/hatex-guide).
* Join the [Mailing List](http://projects.haskell.org/cgi-bin/mailman/listinfo/hatex) for help or announcements of the
latest developments.
* Drop by the IRC channel at `#hatex`.

## TODO list

* Add more examples.
* Add more documentation.
* BibTeX support.

## Related projects

* [haskintex](http://daniel-diaz.github.io/projects/haskintex): Tool to use Haskell (and, additionaly, the HaTeX library)
within a LaTeX file.
* [TeX-my-math](https://github.com/leftaroundabout/Symbolic-math-HaTeX): Experimental library to ease the production
of mathematical expressions using HaTeX (_no longer maintained?_).

## Travis automatic build

For every code push to the GitHub repository, an automatic build checks that the library compiles with several
versions of GHC (7.4, 7.6, and 7.8) and that all tests pass. This label indicates the result of the last automatic
build.

[![Build Status](https://travis-ci.org/Daniel-Diaz/HaTeX.png?branch=master)](https://travis-ci.org/Daniel-Diaz/HaTeX)

Currently, automatic builds are only running under Linux. We hope Travis will support other systems in the future.
