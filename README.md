# HaTeX ReadMe

`HaTeX` is the LaTeX syntax implementation done in Haskell.

Check a list of examples of usage in the [examples](Examples/) directory.
A good starting point may be [simple.hs](Examples/simple.hs).
Run the script using the ``main`` function.

## Installation notes

To install `HaTeX`, use [cabal-install](http://hackage.haskell.org/package/cabal-install).

    $ cabal install HaTeX

This will install the latest official release (recommended).
If you want to try a newer version, use _git_ to clone the code contained
in this repository.

    $ git clone https://github.com/Daniel-Diaz/HaTeX.git
    $ cd HaTeX
    $ cabal install

However, note that the API may be unstable and is subject to any kind of change.
The package version follows the [_Package Versioning Policy_](http://www.haskell.org/haskellwiki/Package_versioning_policy),
so it is unlikely to suffer from API breakages if you follow it too when importing the library.

## HaTeX User's Guide

The HaTeX User's Guide lives [here](https://github.com/Daniel-Diaz/HaTeX-Guide)... and is also done in Haskell!
It is free source and anybody can contribute to it. Doing so, you will help current and future users!

A downloadable version (not necessarily fully updated) can be found [here](http://daniel-diaz.github.com/projects/hatex/HaTeX-Guide.pdf).
To be sure that you are reading the last version, go to the github repository of the guide and follow instructions
to build it. It is fairly easy.

## Contributing

To contribute to HaTeX, please, visit our code repository in GitHub:

https://github.com/Daniel-Diaz/HaTeX

## TODO list

* Add more examples.
* More testing on the parser (See [#15](https://github.com/Daniel-Diaz/HaTeX/issues/15)).
* Add more documentation.
* BibTeX support.

## Packages to be implemented

* fancyhdr
* geometry

## Related projects

* [TeX-my-math](https://github.com/leftaroundabout/Symbolic-math-HaTeX): Experimental library to ease the production
of mathematical expressions using HaTeX.
