coda
====

[![Travis CI](https://secure.travis-ci.org/ekmett/coda.png?branch=master)](http://travis-ci.org/ekmett/coda)
[![Appveyor](https://ci.appveyor.com/api/projects/status/github/ekmett/coda?branch=master&svg=true)](https://ci.appveyor.com/project/ekmett/coda)

This package will eventually provide a toy compiler for experimenting with resumable parsing.

The application is designed as a plugin for Visual Studio Code.

This also serves as an experiment in employing [`shake`](http://shakebuild.com/) inside of a custom cabal `Setup.hs`

Installation
============

To build:

1. Make sure you have `npm` and `Visual Studio Code` installed and that `code` invokes the latter from the command line.

2. Run `cabal install` or `stack install` to build and register the extension with Visual Studio Code.

If `code` is not in your path, or you want a manual install for some reason, you can use `cabal build`, open Visual Studio
Code, type `⌘-Shift-P`, select `> Extensions: Install Extension from VSIX` from the resulting selection box, and finally pick out `dist/build/coda-<version>.vsix` in the finder dialogue to install the package.

The instructions in [Running and Debugging Your Extension](https://code.visualstudio.com/docs/extensions/debugging-extensions)
can be readily tweaked to work here if you need more interactive debugging support when working on the compiler.

Requirements
============

Currently, the build process is being tested on GHC 8.0, but I'm not actively doing anything to shut off older GHCs or newer ones.

Patches that help increase portability are welcome.

Documentation
=============

Once there is an actual language here documentation will be forthcoming on it.

In the meantime, API documentation is available from https://ekmett.github.io/coda/

Contact Information
===================

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the ##coda or #haskell IRC channels on irc.freenode.net.

-Edward Kmett
