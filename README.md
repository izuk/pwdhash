# Overview

This is a Haskell implementation of the algorithm for generating
password hashes described at https://www.pwdhash.com/.

# Usage

The executable is handy for cut-and-paste into password fields.  On
X-windows:

    $ cabal install exe:pwdhash
    $ pwdhash foobar.com | xsel
    Password: *****

# Testing

To run the tests you need nodejs to execute js/pwdhash.js.  Then:

    $ cabal test

# XMonad

TODO: Integrate with XMonad, possibly with XMonad.Prompt.Input and
XMonad.Util.Paste.
