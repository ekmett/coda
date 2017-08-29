# coda

[![Travis Continuous Integration Status][travis-img]][travis]
[![Appveyor Continuous Integration Status][appveyor-img]][appveyor]
[![Circle Continuous Integration Status][circle-img]][circle]

This package will eventually provide a toy compiler for experimenting with resumable parsing.

For now, it provides an entertaining series of crashes and confusing error messages.

The application is designed as a plugin for Visual Studio Code.

**Table of Contents**

- [Installation](#installation)
- [Autocompletion](#autocompletion)
- [Requirements](#requirements)
- [Documentation](#documentation)
- [Directories](#directories)
- [License](#license)
- [Contact Information](#contact-information)

Installation
============

To install the `coda` executable run `cabal install` or `stack install` as usual with a Haskell project.

To work on the extension, you'll need to:

1. Download the repository from <https://github.com/ekmett/coda> if that isn't where you are reading this file from.

2. Run `code .` from root of that repository

3. Start debugging to launch the extension-host following the instructions in [Running and Debugging Your Extension][debugging-extensions].

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

| Directory     | Usage |
| ------------- | ----- |
| .vscode       | Visual Studio Code configuration for the current workspace |
| bin           | Executable scripts |
| code          | The typescript source for the extension |
| images        | The logo, etc. |
| src           | coda library source code |
| test/code     | typescript code for Visual Studio Code |
| test/data     | data files used by the test suite |
| test/shim     | shims to work around doctest limitations |
| test/src      | tasty tests |

License
=======

[BSD-2-Clause](https://opensource.org/licenses/BSD-2-Clause).

See [LICENSE.md](LICENSE.md)

Contact Information
===================

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the ##coda or #haskell IRC channels on irc.freenode.net.

-Edward Kmett

 [appveyor]: https://ci.appveyor.com/project/ekmett/coda
 [appveyor-img]: https://ci.appveyor.com/api/projects/status/github/ekmett/coda?branch=master&svg=true
 [circle]: https://circleci.com/gh/ekmett/coda
 [circle-img]: https://img.shields.io/circleci/project/github/ekmett/coda.png
 [debugging-extensions]: https://code.visualstudio.com/docs/extensions/debugging-extensions
 [shake]: http://shakebuild.com/
 [travis]: http://travis-ci.org/ekmett/coda
 [travis-img]: https://secure.travis-ci.org/ekmett/coda.png?branch=master
