# clr-haskell - Haskell interoperability with the CLR

## Status

[![unix build status](https://gitlab.com/tim-m89/clr-haskell/badges/master/build.svg)](https://gitlab.com/tim-m89/clr-haskell/commits/master)[![Windows Build status](https://ci.appveyor.com/api/projects/status/073rvyuyvxrcqvsw?svg=true&label=Windows%20build)](https://ci.appveyor.com/project/TimMatthews/clr-haskell)

Overview
==========

**clr-haskell** is a project to enable the use of code within the common language runtime (.Net / Mono / CoreCLR) from GHC Haskell.

This project is not a way to target / cross compile Haskell to run on the CLR, instead the Haskell code compiles & runs as normal, and so does the CLR code.

It merely allows these 2 eco systems to interface to each other.

### Flavours


This project provides 2 primary flavours for a developer to interop between the CLR & Haskell:

* The Haskeller's strongly typed flavour. Takes advantage of the latest GHC extensions to provide a way of encoding an OO type system within the Haskell type system.
* The .Net dev's inline flavour. Provides the ability to call directly into valid C# / F# syntax via quasi-quoted template Haskell.

Project Structure
==========

### Libraries

* [**clr-typed**](https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-typed) - The strongly typed flavour.
* [**clr-inline**](https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-inline) - The inline flavour.
* [**clr-host**](https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-host) - Host (also known as embed) the CLR runtime within the current Haskell process, and minimal boostrap interface for the other libraries.
* [**clr-marshal**](https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-marshal) -  Common dep used by both flavours - marshaling and unmarshaling high level Haskell types to low level bridge types.
* [**clr-bindings**](https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-bindings) - Glue between the strongly typed flavour and the backend interface of the clr-host library.
* [**clr-import-gen**](https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-import-gen) - Work in progress for generating the instances required for the typed flavour via a simple import like syntax

### Utils

* [**clr-win-linker**](https://gitlab.com/tim-m89/clr-haskell/tree/master/utils/clr-win-linker) - A front end to the linker to back-port a bug fix to GHC versions < 8.2

In memoriam
==============

Tim Matthews passed out in the early days of August 2017, 4 months after the first release of the clr packages in April 2017. Without his work this suite of packages would probably not exist today. Tim was also the main proponent of the **clr-typed** frontend, which is no longer in active development.

License
==========

Open source under the permissive BSD3 license. See [LICENSE](https://gitlab.com/tim-m89/clr-haskell/tree/master/LICENSE)



