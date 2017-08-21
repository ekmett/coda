coda
====

[![Build Status](https://secure.travis-ci.org/ekmett/coda.png?branch=master)](http://travis-ci.org/ekmett/coda)

This package provides a toy compiler for experimenting with resumable parsing, which is designed as a plugin for Visual Studio Code.

Installation
============

To build:

1. Make sure you have `npm` and `Visual Studio Code` installed and that `code` from the command line will invoke visual studio code.

2. If you would like to avoid affecting your global environment, you can choose to run

```
$ cabal sandbox
```

or set up a local `stack` configuration.

3. Just

```
$ cabal install
```

to register the extension with Visual Studio. Alternately, you can `cabal build` then 

4. 

4. Install the resulting `dist/build/coda-<version>.vsix` file by opening Visual Studio Code, then hitting
   `⌘-Shift-P` then selecting

```
> Extensions: Install Extension from VSIX
```

and selecting the file.

To side-step this process for active development on the compiler, you can try to symlink the client
directory directly into Visual Studio Code with

```
$ make dirty
```

and then just build in place.

The instructions in [Running and Debugging Your Extension](https://code.visualstudio.com/docs/extensions/debugging-extensions)
can be readily tweaked to work here if you need more interactive debugging support when working on the compiler.

Requirements
============

Currently, the build process is being tested on GHC 8.0, but I'm not actively doing anything to shut off older GHCs or newer ones.

Patches that help increase portability are welcome.

Contact Information
===================

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the ##coda or #haskell IRC channels on irc.freenode.net.

-Edward Kmett
