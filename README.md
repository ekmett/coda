# coda

[![Travis Continuous Integration Status][travis-img]][travis]
[![Appveyor Continuous Integration Status][appveyor-img]][appveyor]
[![Circle Continuous Integration Status][circle-img]][circle]


This package will eventually provide a toy compiler for experimenting with resumable parsing.

The application is designed as a plugin for Visual Studio Code.

This also serves as an experiment in employing [`shake`][shake] inside of a custom cabal `Setup.hs`

**Table of Contents**

- [Installation](#installation)
- [Autocompletion](#autocompletion)
- [Requirements](#requirements)
- [Documentation](#documentation)
- [Directories](#directories)
- [Contact Information](#contact-information)

Installation
============

To build:

1. Make sure you have `npm` and `Visual Studio Code` installed and that `code` invokes the latter from the command line.

2. Run `cabal install` or `stack install` to build and register the extension with Visual Studio Code.

If `code` is not in your path, or you want a manual install for some reason, you can use `cabal build`, open Visual Studio
Code, type `⌘-Shift-P`, select `> Extensions: Install Extension from VSIX` from the resulting selection box, and finally pick out `dist/build/coda-<version>.vsix` in the finder dialogue to install the package.

The instructions in [Running and Debugging Your Extension][debugging-extensions] can be readily tweaked to work here if you
need more interactive debugging support when working on the compiler.

Autocompletion
==============

Once you have an installed `coda` executable, bash command line autocompletion is available with:

```
$ source <(coda --bash-completion-script `which coda`)
```

You can add this to your `.profile` or `.bashrc`

Requirements
============

Currently, the build process is being tested on GHC 8.0, but I'm not actively doing anything to shut off older GHCs or newer ones.

Patches that help increase portability are welcome.

Documentation
=============

Once there is an actual language here documentation will be forthcoming on it.

In the meantime, API documentation is available from https://ekmett.github.io/coda/

Directories
===========

| Directory | Usage |
| --------- | --- |
| bin       | Executable scripts used by CI |
| etc       | Configuration used by CI |
| ext       | Template files used to build the extension |
| ext/test  | The vscoe test suite for the extension |
| main      | Where to find `Main.hs` for executable haskell files |
| src       | coda library source code |
| test/data | data files used by the test suite |
| test/shim | shims to work around doctest limitations |
| test/main | Where to find `Main.hs` for test suites |
| test/src  | tasty tests |
| var       | A cache that may (infrequently) be changed during the build. Can be safely deleted |

Contact Information
===================

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the ##coda or #haskell IRC channels on irc.freenode.net.

-Edward Kmett

 [appveyor]: https://ci.appveyor.com/project/ekmett/coda
 [appveyor-img]: https://ci.appveyor.com/api/projects/status/github/ekmett/coda?branch=master&svg=true
 [circle]: https://circleci.com/gh/ekmett/coda
 [circle-img]: https://circleci.com/gh/ekmett/coda.png?style=shield
 [debugging-extensions]: https://code.visualstudio.com/docs/extensions/debugging-extensions
 [shake]: http://shakebuild.com/
 [travis]: http://travis-ci.org/ekmett/coda
 [travis-img]: https://secure.travis-ci.org/ekmett/coda.png?branch=master
