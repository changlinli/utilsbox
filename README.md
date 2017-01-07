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
