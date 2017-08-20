coda
====

[![Build Status](https://secure.travis-ci.org/ekmett/coda.png?branch=master)](http://travis-ci.org/ekmett/coda)

The entire compiler is being built as a plugin for Visual Studio Code

Build instructions:

```
$ npm install vsce -g
$ npm install --ignore-scripts
$ vsce package
```

this will prdoduce a `.vsix` file that can be installed into Visual Studio Code. The package installer currently assumes that the target
machine has a modern GHC installed and includes the haskell source to be built on the target machine.

We could alternately switch to using ghcjs to build during the devDependency step and run directly in the
client process, but while it'd reduce end-user dependencies, it'd slow build time for extension development.

You can also set up a development environment for debugging the extension during extension development.
See [Building, Debugging and Sideloading the extension in Visual Studio Code](https://github.com/Microsoft/vscode-go/wiki/Building,-Debugging-and-Sideloading-the-extension-in-Visual-Studio-Code)

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
