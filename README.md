# clr-haskell: Interoperability with the CLR

[![unix build status](https://gitlab.com/tim-m89/clr-haskell/badges/master/build.svg)](https://gitlab.com/tim-m89/clr-haskell/commits/master)[![Windows Build Status](https://img.shields.io/appveyor/ci/pepeiborra/clr-haskell.svg?label=Windows%20build)](https://ci.appveyor.com/project/pepeiborra/clr-haskell)

clr-haskell is a project to enable the use of code within the common language runtime (.Net / Mono / CoreCLR) from Haskell. It is direct continuation of the Salsa project in that it aims to provide a way to host the runtime within a Haskell process, and a strongly typed binding to any of the code within the CLR. In doing so, it demonstrates that Haskell's type system (GHC specifically) is strong enough to encode most (if not all) the complexities of an OO type system.

We should expect at a bare minimum, a high level interface to call a CLR method by name, and the exact method invocation to go to the right method (virtual methods, overrides of those, methods marked new to hide the base implementation at compile time instead of runtime, overloading: multiple choices based on different numbers and types of arguments, implicit upcast from subclasses to super classes as simple as an original .Net invocation would look, etc)

This project is not a way to target / cross compile Haskell to run on the CLR. The Haskell code remains as is, and so does the CLR code. It is merely trying to bridge functionality between the 2 eco systems.

It is currently divided into these packages:

* clr - Most of the end developer API and lots of type level trickery to make this all work.
* clr-host - Attempts to provide a common minimal disconnected way to start and stop the backend. Mono works great. .Net has a few issues. CoreCLR is not yet implemented but my minimal tests show that it doesn't suffer from all the problems of .Net.
* clr-bindings - Provides glue between the above 2, as neither of those packages depend on each other, some of which is just a few instances.
* clr-inline - A quasiquoter allowing to inline F# and C# in Haskell source code.
* clr-test-app - Just an example of what currently works.
