Elevence Haskell Style Guide
============================

This is a short document describing the preferred coding style for Elevence
Haskell code. It is based on Johan Tibell's
[`haskell-style-guide`](git@github.com:tibbe/haskell-style-guide.git).


Guiding Principles
------------------

* When in doubt optimize for readability and ease of reasoning about code.

* Change-aware coding: we acknowledge that all code is going to be changed in
  the future. We simplify reasoning about changes and enlist the compiler
  where possible to guide us through changes.

* We prefer code layout that scales to large numbers of functions and modules.


Formatting
----------

### Line Length

Maximum line length is *80 characters*.

### Indentation

Tabs are illegal. Use spaces for indenting. We use *4 spaces* by default for
indenting and make use half-indents of 2 spaces for punctuation and cases of
case distinctions. We also indent the `where` keyword with two spaces to set
it apart from the rest of the code and indent the definitions in a
`where` clause 2 spaces. Some examples:

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise =     filter p xs

greetingLines :: Maybe String -> [String]
greetingLines mbName =
    -- Here we use half-indents to save indentation space. The idea is that
    -- prefixed punctuation characters are light-weight enough to serve as
    -- almost an indentation space.
    case x of
      Nothing -> ["Hello John Doe."]
      Just name ->
        [ "Hello " <> name
        , "It is a pleasure to get to know you."
        , "We hope you enjoy your stay at hotel Transylvania."
        ]
```

Try to avoid using indentation whose depth depends on the length of an
identifier, i.e., use

```haskell
foobar
    :: Foo a
    => a -> IO ()
foobar x = do
    a <- alloca x
    b <- alloca 20
    cFunction a b
```
instead of

```haskell
foobar :: Foo a
       => IO ()
foobar = do a <- alloca 10
            b <- alloca 20
            cFunction a b
```

### Blank Lines

One blank line between top-level definitions.  No blank lines between
type signatures and function definitions.  Add one blank line between
functions in a type class instance declaration if the functions bodies
are large.  Use your judgment.

### Whitespace

Surround binary operators with a single space on either side.  Use
your better judgment for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator.  Don't insert a space after a lambda.

### Data Declarations

Align the constructors in a data type definition on the following line.
Example:

```haskell
data Tree a
    = Branch !a !(Tree a) !(Tree a)
    | Leaf
```

Format records as follows:

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```


### Pragmas

Put pragmas immediately before the function/constructor/field they apply to.
Example:

```haskell
{-# INLINE id #-}
id :: a -> a
id x = x
```


### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda.  Use
your judgment. Some examples:

```haskell
bar :: IO ()
bar =
    forM_ [1, 2, 3] $ \n -> do
        putStrLn "Here comes a number!"
        print n

foo :: IO ()
foo =
    alloca 10 $ \a ->
    alloca 20 $ \b ->
    cFunction a b
```

### Export Lists

Format export lists as follows:

```haskell
module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
```

### If-then-else clauses

Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).

When writing non-monadic code (i.e. when not using `do`) and guards
and pattern matches can't be used, you can align if-then-else clauses
like you would normal expressions:

```haskell
foo =
    if ...
    then ...
    else ...
```

Otherwise, you are free to choose between 2 and 4 spaces to indent, but make
sure that the `then` and the `else` keywords are aligned.  Examples:

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```

```haskell
foo = bar $ \qux ->
    if predicate qux
      then doSomethingSilly
      else someOtherCode
```

The same rule applies to nested do blocks:

```haskell
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
```

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

or as

```haskell
foobar = case something of
           Just j  -> foo
           Nothing -> bar
```

Align the `->` arrows when it helps readability.


Imports
-------

Imports should be sorted alphabetically and grouped by top-level
module-hierarchy name. Align common keywords per import group and break
explicit import lists as follows.

```haskell
import           Control.Lens
                 ( preview, ix, at, traverseOf, toListOf
                 , view, use
                 )
import qualified Control.Monad.Catch as Catch
```

Always prefer explicit import lists or `qualified` imports.  This makes the
code more robust against changes in the imported modules.

For qualified imports you should either use the full or abbreviated name of
the last name(s) in the module hierarchy. Here are a few examples.

```haskell
import qualified Control.Monad.Catch  as Catch

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
import qualified Data.HashMap         as HM
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
```

There are two exceptions to the qualified import rule. First, our custom
prelude `Elevence.Prelude` can always be imported unqualified. Second,
qualified imports of `.Extended` versions of modules from third-party
libraries (see Section "Common Patterns") can be abbreviated without
mentioning the `.Extended` in the abbreviated name.



Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### Top-Level Definitions

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send
    :: Socket      -- ^ Connected socket
    -> ByteString  -- ^ Data to send
    -> IO Int      -- ^ Bytes sent
```

For functions the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments format them like so:

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text

      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces.  Align
comments for data type definitions.  Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### Links

Use in-line links economically.  You are encouraged to add links for
API names.  It is not necessary to add links for all API names in a
Haddock comment.  We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your judgment), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.

### Abbreviations

Avoid unprincipled abbreviations; in particular when naming top-level
functions. If you must abbreviate an identifier, then use the sequence of its
initial letter and all following capital-case letters as the abbreviation.

### Records and Constructors

Prefix record fields either with the full name of the type or with the
abbreviated name of the type. For example,

```haskell
data EmailAddress = EmailAddress
    { eaName   :: !Text
    , eaDomain :: !Text
    }
```

or

```haskell
data EmailAddress = EmailAddress
    { emailAddressName   :: !Text
    , emailAddressDomain :: !Text
    }
```

If you need to disambiguate record constructors, then do this by post-fixing
either the full or abbreviated name of the type. For example,

```haskell
data ValidationError
    = ReferenceVE !Reference
    | CharacterVE !Char
```

or

```haskell
data ValidationError
    = ReferenceValidationError !Reference
    | CharacterValidationError !Char
```


### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

### Name spaces

Avoid repeating a module's name in the name of the types and values it is
defining. In particular avoid abbreviating the actual interesting part of the
name in favor of repeating the module name. Modules form name spaces that
should be made use of. For example,

```haskell
-- Bad
module Foo.Bar where

data BarS = A | B
```

```haskell
-- Good
module Foo.Bar

data State = A | B
```

Dealing with laziness
---------------------

By default, use strict data types and lazy functions.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you can put

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file. Including this flag in the file itself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`). If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

Misc
----

### Point-free style ###

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

### Warnings ###

Code should be compilable with `-Wall -Werror`. There should be no
warnings.


### FIXMEs, TODOs, and NOTEs

- We use `FIXME (author-name): what-to-fix` to mark pieces of code that must be
  fixed before being merged into `master`.
- We use `TODO (author-name): what-to-do` to mark explain future
  extensions/changes to the code that might be worthwhile.
- We use `NOTE (author-name): note` to annotate a piece of code with
  explanations that help understanding this code.

We add the author-name to each of these elements, as we always want to make
sure that one knows whom to ask in case further clarification is needed. If a
`FIXME` addresses a very specific person that has to fix it then we state this
be referencing this person in the explanation of what has to be fixed.


Design Patterns
---------------

### Orphan Instances

All our orphan instances are grouped per library in
`Orphans.Lib_<library-name>` modules, which in turn are all imported in
`Elevence.Prelude`. This makes sure that we do not have any duplicate orphan
instance definitions. This ensures that we stay away from the orphan-instance
hell with conflicting, hidden orphan instances.


### .Extended modules

We adapt third-party libraries using the
[`.Extended` modules pattern](http://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html).

### The service pattern

We structure our IO layer using the
[service pattern](https://www.fpcomplete.com/user/meiersi/the-service-pattern).

### Modules with batteries included

We prefer working with modules that come with batteries included, i.e., where
a single import brings all useful library functions into scope. This reduces
import clutter.

