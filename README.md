Monad-Finally
=============

[![Build](https://github.com/mvv/monad-finally/actions/workflows/ci.yml/badge.svg)](https://github.com/mvv/monad-finally/actions/workflows/ci.yml) [![Hackage](https://img.shields.io/hackage/v/monad-finally.svg)](http://hackage.haskell.org/package/monad-finally)

This package provides a generalized version of `Control.Exception.finally`.
The cleanup action is run not only on successful termination of the main
computation and on errors, but on any control flow disruption (e.g.
`mzero`, short-circuiting) that causes the main computation to not produce
a result.

Installation
------------
The usual:

	$ cabal install

