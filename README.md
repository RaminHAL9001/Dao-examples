# Dao Example Programs
Copyright (C) 2008-2015, Ramin Honary, all rights reserved.

## Example programs demonstrating how to use the Dao package

Each Haskell program in this package is a module which explains some
aspect of how to use the Dao package. To compile all of the modules in
this package, just run Cabal, no need to install it:

```bash
cabal configure && cabal make
```

The documentation is written in Haddock markup in the source code, so
reading through the documentation of each module from beginning to end
will explain the logic of how the program was constructed. Running

```bash
cabal haddock
```

Will generate the HTML documentation in the
`./dist/doc/html/Dao-examples` directory, which may be easier to read
than the source code alone. Source code is included in the Haddock
documentation.

The .ghci file is provided so you can load each module in GHCi and
experiment with it.

Modules build on one another, so it is a good idea to read each module
as though it is a chapter in a book.

### Table of Contents

1. **FuzzyString:** a custom data type instantiating the dynamic data
   type class 'Object'. It is shown how to create data types for
   defining production rules in a knowledge base.
2. **Numbers:** The FuzzyString module is used to create a production
   rule knowledge base that parses integers expressed as an English
   language phrase, and is not easily confused by misspelled words.
3. Coming soon...

Dao-examples is a Haskell package (library) licensed under the GNU
Affero General Public License:
	http://www.gnu.org/licenses/agpl.html

