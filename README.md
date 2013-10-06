# The HaTeX library

HaTeX is a Haskell library that implements the *LaTeX syntax*.

Check a list of examples of usage in the [Examples](https://github.com/Daniel-Diaz/HaTeX/tree/master/Examples) directory.
A good starting point may be [simple.hs](https://github.com/Daniel-Diaz/HaTeX/blob/master/Examples/simple.hs).
Run any example script executing the ``main`` function.

## Installation notes

To install `HaTeX`, use [cabal-install](http://hackage.haskell.org/package/cabal-install).

    $ cabal update
    $ cabal install HaTeX

This will install the latest official release (recommended).
If you want to try a newer version, use _git_ to clone the code contained
in this repository.

    $ git clone git@github.com:Daniel-Diaz/HaTeX.git
    $ cd HaTeX
    $ cabal install

However, note that the API may be unstable and is subject to any kind of change.
In the other hand, this package follows the [_Package Versioning Policy_](http://www.haskell.org/haskellwiki/Package_versioning_policy),
so it is unlikely to suffer from API breakages if you follow it too when importing the library.

## Travis automatic build

[![Build Status](https://travis-ci.org/Daniel-Diaz/HaTeX.png?branch=master)](https://travis-ci.org/Daniel-Diaz/HaTeX)

## HaTeX User's Guide

The HaTeX User's Guide lives [here](https://github.com/Daniel-Diaz/hatex-guide)... and is also done in Haskell!
It is free source and anybody can contribute to it. Doing so, you will help current and future users!

A downloadable version (not necessarily the last version, but most likely)
can be found [here](http://daniel-diaz.github.com/projects/hatex/hatex-guide.pdf).
To be sure that you are reading the last version, go to the github repository of the guide and follow instructions
to build it. It is fairly easy.

## Community and Contributions

There are many ways to get involved in the HaTeX project.

* Fork the [GitHub repository](https://github.com/Daniel-Diaz/HaTeX).
* Report bugs or make suggestions opening a ticket in the [Issue Tracker](https://github.com/Daniel-Diaz/HaTeX/issues).
* Help us to improve and extend our [hatex-guide](https://github.com/Daniel-Diaz/hatex-guide).
* Join the [Mailing List](http://projects.haskell.org/cgi-bin/mailman/listinfo/hatex) for help or announcements of the
last developments.
* Drop by the IRC channel at `#hatex`.

## TODO list

* Add more examples.
* More testing on the parser (See [#15](https://github.com/Daniel-Diaz/HaTeX/issues/15)).
* Add more documentation.
* BibTeX support.

## Packages to be implemented

* geometry

## Related projects

* [TeX-my-math](https://github.com/leftaroundabout/Symbolic-math-HaTeX): Experimental library to ease the production
of mathematical expressions using HaTeX.
* [haskintex](https://github.com/Daniel-Diaz/haskintex): Tool to use Haskell and in particular the HaTeX library
within a LaTeX file.
