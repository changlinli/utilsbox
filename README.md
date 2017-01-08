UtilsBox
========

A coreutils-like collection of utilities that is called in the same way that as
Busybox is; that is it is a single executable with many subcommands which can be
executed either an argument to the executable, or by calling the execuatble by a
certain name.

    utilsbox true
    # or equivalently
    ln -s /$UTILSBOX_INSTALL_LOCATION/utilsbox true
    true

Why?
----

There are two reasons for the existence of these programs. The first is that we
have a suite of tools that can run in any environment that Javascript can. This
is especially useful for having a toy web-based console that you can use to play
around with things on a virtual filesystem.

Another reason is to explore how well free monads work in a "real life" system.
Coreutils exercises enough interesting functionality that we can explore how
IO-heavy systems work in a free monad world, as well as investigate performance
hits and how robustness in general works.

Building the Project
--------------------

### Using Nix

The easiest way to build both the GHC and GHCJS parts of this project is to use
Nix. To pull down all the dependencies for GHC, we can simply use the following:

    nix-shell .

To pull down all the dependencies for GHCJS, change the compiler to "ghcjs."

    nix-shell --arg compiler "\"ghcjs\"" .

### Using Stack

If you have `stack` installed, you can run the following from within this
repository.

    stack setup
    stack build
    stack exec utilsbox

### Using Cabal

If you prefer to use `cabal` sandboxes, you can run the following from within
this repository.

    cabal sandbox init
    cabal configure
    cabal build
    cabal run
