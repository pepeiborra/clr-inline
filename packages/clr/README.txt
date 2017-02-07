This is a work in progress Haskell <-> CLR bridging library (.Net / Mono / CoreCLR)

This project is to continue with the strong typed OOP in Haskell encoding that was
previously done in the Salsa library, but to diverge with some new ideas that could
have been done directly in Salsa, and others that are best achieved starting fresh:

* Most notable, Salsa has got a bit of name-space problem. Types are prefixed with an
'_' to reduce risk of collisions. Casing problems could occur but are mostly avoided due
to C sharp's common practice of starting names with upper-case. Then extra declarations
are required to refer to these types within Haskell's value level, and this means a
lower case wrapper is required to avoid to avoid using undefined :: T by the end user.

* Our solution here is to use type level Symbols so that we can refer to a foreign type
names exactly as they are without collisions nor worrying about casing. We use visible
type application to refer to these with Haskell's value level, rather than having extra
witness parameters (proxies).

* The low level declarations within Salsa are declared with particular high level
Haskell types such as String, and marshalling happens at an approximate level.

* The plan here is to declare a .Net type that requires a "System.String" as such with
a particular bridge type (such as CString), and have the marshalling performed at a much
higher level. Then we get a unified implicit interface for both String as Text for the
end user, providing that both of these have Marshal instances.

* Salsa has no support for generics, and it might not be that simple to bolt on.

* Since generics are such an important part of the .Net eco system, the support is
intended to be ironed out near the start, even before this library will actually
interface to the CLR.

* Salsa has an involved manual build process making it somewhat out of reach, and not
good for Hackage / Stackage.

* The plan here is to make it completely automated. Where Salsa requires using the
generator executable to generate particular bindings, the vision is to have a template
Haskell import syntax to achieve this. Since template Haskell can perform arbitrary IO,
we can utilise this to query the CLR GAC of the imported bindings.



