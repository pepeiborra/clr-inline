This package was an attempt at producing a small utility as a wrapper around the linker, so that the following fix can be easier tested in earlier versions of GHC:

https://ghc.haskell.org/trac/ghc/ticket/12186

To build & install, run the following

    stack install

Which will copy the resulting executable to the path, usually 'C:\Users\<name>\AppData\Roaming\local\bin'

Then use from a package such as 'clr-test-app' like so:

    stack build --ghc-options="-pgml clr-win-linker"

And observe then when running the resulting executable, the program gets past the bug behind ghc issue #12186 but segfaults for some other reason:

    stack exec clr-test-app

Note: for me at least, the failure seems kind of silent when run from a git bash terminal, but normally loud when run from a usual cmd window or double clicking the exe.
