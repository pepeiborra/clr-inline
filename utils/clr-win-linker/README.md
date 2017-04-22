This package provides small utility to backport GHC bug fix [#12186](https://ghc.haskell.org/trac/ghc/ticket/12186) for GHC versions < 8.2

It works by being specified as the linker, where it will simply do a find and replace on the supplied linker args before passing the result on to the real linker.

To build & install, run the following:

    stack install clr-win-linker

Which will copy the resulting executable to the path, usually 'C:\Users\<name>\AppData\Roaming\local\bin'

Then specify as the linker when building other packages like so:

    stack build --ghc-options="-pgml clr-win-linker"

And the resulting executables should then get past ghc issue #12186:

    stack exec clr-test-app
