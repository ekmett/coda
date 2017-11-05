# coda

[![Travis Continuous Integration Status][travis-img]][travis]

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
| `.vscode`     | Visual Studio Code configuration for the current workspace |
| `bin`         | Executable scripts |
| `coda*`       | Haskell code for the language |
| `code`        | Typescript code for the extension |
| `images`      | The logo, etc. |
| `test/code`   | Typescript code for testing |

License
=======

[BSD-2-Clause](https://opensource.org/licenses/BSD-2-Clause).

See [LICENSE.md](LICENSE.md)

Contact Information
===================

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the ##coda or #haskell IRC channels on irc.freenode.net.

-Edward Kmett

 [debugging-extensions]: https://code.visualstudio.com/docs/extensions/debugging-extensions
 [shake]: http://shakebuild.com/
 [travis]: http://travis-ci.org/ekmett/coda
 [travis-img]: https://secure.travis-ci.org/ekmett/coda.png?branch=master
