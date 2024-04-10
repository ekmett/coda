# coda

[![haskell ci](https://github.com/ekmett/coda/actions/workflows/ci.yml/badge.svg)](https://github.com/ekmett/coda/actions/workflows/ci.yml)

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

 What is all this about?
 ====================

*This is the response from ekmett to this question on [reddit](https://www.reddit.com/r/programming/comments/bip6ho/graydon_hoare_on_21_compilers_a_wander_through_a/em5grcg/):*

The short version of what coda is about is fixing a number of things that keep functional programming from scaling.

I'm leaving what particular dimension 'scaling' applies to as a free variable.

One way we can fill in that variable is to talk about execution speed:

FP has been good at getting us code that scales down to the core level, but we aren't getting more cores terribly fast. Moore's law used to equate to speed, then it equated to speed * cores, now it mostly equates to speed * cores * width of the SIMD processing unit when it applies at all. I'm interested in ways to apply SPMD-on-SIMD evaluation techniques to functional programming. These are the techniques that make the Intel SPMD Program Compiler work as well as it does. It also provides a window onto how to execute this sort of code on GPU and TPU style hardware. If I want symbol pushing techniques to be at all relevant in a world where these are becoming the norm, we can't fail to scale past the core.

One way we can fill in that variable is to talk about scalability of abstraction:

The typeclass mechanism Haskell uses favors finding a few good abstractions over accurate abstraction selection. If I tried to make the standard library of Haskell have a mathematical concept of a 'Field' and put 600 superclasses with all of the algebra I know above that point, the community would mutiny on day 1. On day 30 when I found it should be 601, and went to make the change all of that code would break. How can we make deep extensible type classes with typeclass hierarchies that actually branch out and aren't linear chains?

One way we can fill in that variable is to talk about scalability of the library ecosystem:

Another dimension where I don't see it scaling is that there are a number of language design decisions in Haskell that keep me from scaling. I maintain a rather ridiculous number of Haskell libraries, all of which have to work together. Haskell is pretty darn good at solving diamond dependency issues, compared to something like node, which just ignores the problem, in a way that makes it easy to depend on ridiculous numbers of dependencies, but building a haskell project still consists of spending 97%+ of your time compiling instances for classes that never occur in the final program in a way that is forced by the structure of package management. You have to supply instances for every package that came before you in the Haskell ecosystem or you are just incompatible because of uniqueness of instance resolution. Orphan instance packages can be forgotten or mis-used. How do we fix this problem?

One way we can fill in that variable is to talk about scalability of proof effort:

It took 13000 lines of basically nonreusable code, 9 months worth of effort and a PhD to prove the correctness of the GMP sqrt function. Most issues I'm interested in are bigger than that.

There are other directions to explore here.

How to scale in terms of parallelizing team efforts? e.g. Letting more people work on proof infrastructure.

How to factor apart what it means to prove something correct, and finding a space of equivalent, correct programs, and then to make it fast by selecting tactics style out of a space of equivalent correct programs the one you want?

How to throw program synthesis techniques at problems to increase the grain size of problems that the compiler just handles for you? The work on synquid by Nadia Polikarpova, Barliman by Will Byrd, and Blodwen by Edwin Brady all point generally at something in this space. Each offers different ways to prune the search space drastically by using types or different generation techniques.

How to scale the number of machines that are used for synthesis? I know tons of people who work in rendering films. There we spin up 10k+ machines to save artist time. Is developer time less valuable? This also drastically changes the scope of what is considered synthesizable when this is your baseline.

Most of my efforts lately have gone into this subgoal of program synthesis, hence coda itself has languished while I build the pieces there that I need to build the type checker infrastructure on top of. The irony is not lost upon me that this languishing arises largely due to my inability to scale up my own local development efforts, personally.
