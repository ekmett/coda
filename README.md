# coda

[![Travis Continuous Integration Status][travis-img]][travis]

This package will eventually provide a toy compiler.

For now, it provides an entertaining series of crashes and confusing error messages.

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

To install the `coda` executable, you will need GHC 8.4.1 release candidate 1 or later, and you'll need to run `cabal new-build`, and fish through `dist-newstyle` for the executable and put it somewhere in your path.

Once `cabal new-install` works, this will become a lot easier.

To work on the extension, you'll need to:

1. Download the repository from <https://github.com/ekmett/coda> if that isn't where you are reading this file from.

2. Run `code .` from root of that repository

3. Start debugging to launch the extension-host following the instructions in [Running and Debugging Your Extension][debugging-extensions].

Alternately you can link the directory directly into your `~/.vscode/extensions` folder, which may be useful if you're working on the typescript bits.

Autocompletion
==============

Once you have an installed `coda` executable, bash command line autocompletion is available with:

```
$ source <(coda --bash-completion-script `which coda`)
```

You can add this to your `.profile` or `.bashrc`

Requirements
============

Currently, the build process is being tested on GHC 8.2, but I'm not actively doing anything to shut off older GHCs or newer ones.

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
| `lib/coda*`   | Haskell code for the language |
| `code`        | Typescript code for the extension |
| `images`      | The logo, etc. |
| `test/code`   | Typescript code for testing |

License
=======

[Licensed](LICENSE.md) under either of
 * Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
 * BSD 2-Clause license (https://opensource.org/licenses/BSD-2-Clause)
at your option.

Contribution
============

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual-licensed as above, without any
additional terms or conditions.

Contact Information
===================

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the ##coda or #haskell IRC channels on irc.freenode.net.

-Edward Kmett

 [debugging-extensions]: https://code.visualstudio.com/docs/extensions/debugging-extensions
 [shake]: http://shakebuild.com/
 [travis]: http://travis-ci.org/ekmett/coda
 [travis-img]: https://secure.travis-ci.org/ekmett/coda.png?branch=master
