% Haskell and FP in industry
% Dr. Simon Meier, Digital Asset
% December 1st, 2016


# About me

- learned to programming Basic (1995)
- learned about Haskell during my ETH CS studies (2003)
- first "commercial" Haskell development during PhD (2007 - 2012)
- Haskell software engineer/architect at Better AG (2013 - 2014)
- post-doctoral researcher at IBM Research (2014 - 2015)
- Haskell software engineer/architect at Elevence Digital Finance AG (2015 - 2016)
- FP and formal methods expert at Digital Asset (2016 -)


# Goals of this talk

- explain why I use Haskell and FP in my professional career
- convey some of the lessons I learned
- provide pointers for further learning


# Commercial Haskell development - my view on it


![](img/business-model.jpg)

We'll want to be conscious about our business model.

See [this post-mortem of Klimpr](https://medium.com/@adriankyburz/it-s-been-an-amazing-ride-now-my-startup-is-dead-and-here-s-what-i-ve-learned-284e14ef4ee0#.w4621izgf),
a friend's startup, for further insights.


# We are always selling a user experience

![](img/user-experience-for-startups.png)

Haskell is just a tool in our box.


# We're only done once we've shipped our code

![](img/spacex-launch.jpg)

Deployment and coding are equally important.


# We want to use the right tools for the jobs at hand

![](img/cnc_machine.jpg)

Haskell is a great tool for many software engineering tasks.

However not all problems are best solved using Haskell or FP.


# Writing code in an agile world

![](img/agile_coding.jpg)

Our guidelines at Digital Asset:

- We acknowledge that all code is going to be changed in the future. We
  simplify reasoning about changes and enlist the compiler where possible to
  guide us through changes.
- When in doubt we optimize for readability and ease of reasoning about code.


# Reasoning about code

- type system: the compiler takes care of checking value-flow
- purity: proofs become context-independent
- totality: functions are forced to work in all contexts
- mechanized theorem proving:
  - learn when a proof really is a proof
  - great education even if you cannot yet use it in practice


# Reasoning about code in Haskell

- type system that guarantees purity
- totality requires a bit of discipline
- theorem provng: in the works
  - [Refinement Types](https://github.com/ucsd-progsys/liquidhaskell)
  - [Dependent Haskell](https://github.com/goldfirere/thesis/blob/master/built/thesis.pdf)
  - [HipSpec](https://github.com/danr/hipspec)
  - [Haskabelle](https://isabelle.in.tum.de/haskabelle.html)


# Reasoning about code in general

Some pointers:

  - [Isabelle/HOL](https://isabelle.in.tum.de/)
  - [Coq](https://coq.inria.fr/), [Book on formally verified PLs](https://www.cis.upenn.edu/~bcpierce/sf/current/toc.html)
  - [Satisfiability module Theories (SMT)](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories): [Z3](https://github.com/Z3Prover/z3)
  - [Lean](http://leanprover.github.io/): combine interactive and automated theorem proving
  - [CompCert](http://compcert.inria.fr/): a verified C compiler
  - [CakeML](https://cakeml.org/): a verified ML compiler
  - [SeL4 Verified Microkernel](https://sel4.systems/)
  - [High-Assurance Cyber Military Systems](http://www.darpa.mil/program/high-assurance-cyber-military-systems)
  - [TLA+](http://research.microsoft.com/en-us/um/people/lamport/tla/tla.html) use at
    [Microsoft](https://www.microsoft.com/en-us/research/publication/ironfleet-proving-practical-distributed-systems-correct/) and
    [Amazon](http://research.microsoft.com/en-us/um/people/lamport/tla/amazon.html)


# Industrial Haskell programming

- know thy version control
  - monorepo
  - carefully tailor repo layout to your needs

- know thy tooling
  - `ghcid`
  - `stack`

- know thy [Hackage](https://hackage.haskell.org/)
  -

- know thy books and blogs
  - [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)
  - typeclassopedia
  - concurrent haskell
  - principled haskell programming


- know thy patterns
  - style guide
  - base
  - .Extended
  - Service pattern
  - Haskell mistakes I made

# The Haskell community

you're not the only one that wants his code to behave!

- HaskellerZ, FP Syd
- ZuriHac, MuniHac, BayHac, FP Syd, HacPhi
- reddit
- IRC

never give up on reasoning about your code!







# Questions by Farhad




- Why did you choose Haskell for your professional work?


  - I want to build software that works
  - never loose ability to reason about code
    - pure and total functions are key

  - Haskell is currently the only industrial language that supports this style

- Looking back, what issues made you glad that you took this decision, and
  what issues made you regret taking this decision?

  - Haskell programming plus interactive theorem proving are great guides
    to understand the core of software engineering
  - people that care about principled approaches are drawn to Haskell, the
    problems that you can tackle are very interesting
  - I don't regret this decision. It brought me to the place I am and I think
    it provides a great step towards software that works in all forms

- If FP is so great, why isn't everyone using it? What advances, do you
  feel, would work towards the industry better reaping the benefits of FP?
  (e.g. training, better tools, etc.)

  - it is a very different approach. purity is so easily lost. common argument
    of JVM-based languages that they have all the libraries. Most of them are
    stateful and compose badly.
  - education in formal methods and theorem proving; i.e., how to write
    correct code
  - easy means to combine correctness and performance; Rust is a great example

- Do you think FP will become more mainstream in the future?
  - yes I definitely think so; Spark, React.js, Java 8, C++14



# This talk

Amalgamate of
- A primer to commercial Haskell programming
- Beginner Haskell Mistakes by Jasper van der Jeugt

Main goal: provide a launchpad for your future Haskell endeavours



# Talk Outline

- code layout
- the `stack` tool
- the custom-prelude pattern
- the .Extended-pattern
- style guidelines
- additional resources


# Code layout

Mono-repositories are great.

- transactional changes across all of your
  software artifacts
- see [this](http://danluu.com/monorepo/) and
     [that](https://github.com/babel/babel/blob/master/doc/design/monorepo.md)
  for more in-depth comparison of mono- vs multi-repo setups

**Big thanks to Elevence for publishing their
  [development repo layout](https://github.com/meiersi/HaskellerZ/tree/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence)**
  including

- their `elevence-base` library and
- their style-guide.


# The stack tool


- solid multi-library and application support
- good build reproducability
- lots of options,
  [which one should know about](http://docs.haskellstack.org/en/stable/GUIDE.html).

In my opinion, `stack` is a must use for commercial Haskell development.

([Nix](http://www.cse.chalmers.se/~bernardy/nix.html) is an alternative,
but has a way higher ramp-up cost.)


# The custom-prelude pattern

Key purposes of our [`Elevence.Prelude`](https://github.com/meiersi/HaskellerZ/tree/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence/libs/hs/elevence-base/src/Elevence/Prelude.hs):

- reduce import clutter for standard modules
- provide an insulation layer against changes in GHC/Hackage
- provide a principled way of introducing orphan instances
    - introduce `Orphans.Lib_<library>` modules for defining orphan instances
      for classes defined in `<library>`
    - import all of these in `Elevence.Prelude`
    - guarantees instance coherence, as `Elevence.Prelude` is always in scope

I believe that every Haskell company should use their own prelude.


# The .Extended pattern

Similar goals as the custom-prelude pattern,
but for individual libraries on Hackage.

- reduce import clutter
    - for libraries whose exports are too fine-grained, or
    - for functionality spread over multiple libraries
      (e.g., `lens` support for `text` values).
- provide an insulation layer for changes in Hackage libraries

See [Jasper Van der Jeugt's post for a detailed explanation](https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html).


# The Elevence Haskell style guidelines

[Let's have a look at them](https://github.com/meiersi/HaskellerZ/blob/master/meetups/20160128-A_primer_to_commercial_Haskell_programming/code-by-elevence/docs/hs-style-guide.md).


# Additional Resources

There are a lot of resources, but they are not as condensed as one might like.

I believe Stephen Diehl's
[What I wish I knew when I learned Haskell](http://dev.stephendiehl.com/hask/)
provides a great entry point.

For people that are more freshly starting with Haskell,
there's also our [HaskellerZ meetup repo](https://github.com/meiersi/HaskellerZ)
and [my talk on getting started with Haskell](https://github.com/meiersi/HaskellerZ/blob/master/meetups/2015-02-26-Writing_your_first_real_world_Haskell_application/real_world_haskell_intro.markdown).




#Haskell: Mistakes I’ve made HaskellerZ

Jasper Van der Jeugt March 31, 2016

About me
- I really like Haskell
- Contributed to and authored open source projects
- Did some consulting for various companies
- Worked at Better, currently at fugue.co

## what this talk is about

Beginner Haskell “Mistakes” that can be avoided, signs of Haskell code smell.

## What this talk is not about

We only talk about code that is accepted by the compiler.
``
  fib :: Int -> Int
  fib n =
    | n <= 1    = 1
    | otherwise =
        fib (n - 1) +
        fib (n - 2)
``


# Intro: pick your battles

 Pick your battles

It’s often impossible to have perfectly clean code everywhere in large projects.

 Pick your battles

- Aesthetically pleasing code?
- Just need to get the job done?
- Sacrifice clean code on the performance altar?
- Document top-level functions?

# Pick your battles
What works for me: focus on modules as units.

# Pick your battles: modules
The exposed interface of a module should be clean.
But inside you can `unsafePerformIO` all you want.

# Pick your battles: modules

The module should have some documentation about what it is for and how one should use it.
But you don’t necessarily need to write Haddock for all functions if it’s clear what they do from the name.

# Pick your battles: modules

There should be some tests that verify that the module works correctly.
But you don’t need to test all the internals.

# Mistake: not using -Wall

# Mistake: not using -Wall

Always compile using `-Wall`.

Use `-Werror` for production/tests.

# Low-risk -Wall
`-fwarn-unused-imports`,
`-fwarn-dodgy-exports`,
`-fwarn-dodgy-imports`,
`-fwarn-monomorphism-restriction`,
`-fwarn-implicit-prelude`,
`-fwarn-missing-local-sigs`,
`-fwarn-missing-exported-sigs`,
`-fwarn-missing-import-lists`,
`-fwarn-identities

# Medium-risk -Wall

-fwarn-unused-binds,
-fwarn-unused-matches,
-fwarn-auto-orphans

# High-risk -Wall
-fwarn-incomplete-patterns
-fwarn-incomplete-uni-patterns
-fwarn-incomplete-record-updates

# Mistake: wildcard pattern matches


  data Murderer
    = Innocent
    | Murderer
  canBeReleased
    :: IsMurderer -> Bool
  canBeReleased = \case
    Innocent -> True
    Murderer -> False
 Mistake: wildcard pattern matches
fireSquad :: IsMurderer -> Bool
fireSquad = \case
  Innocent -> False
  _        -> True
 Mistake: wildcard pattern matches
data IsMurderer
  = Innocent
  | Murderer
  | Like99PercentSure
 Mistake: wildcard pattern matches
Note: this also appears in other forms!
fireSquad :: IsMurderer -> Bool
fireSquad x = x /= Innocent
 Mistake: wildcard pattern matches
It is often best to explicitly match on every constructor, unless you can be sure no further constructors will be added ( Maybe , Either ...).
 Mistake: you think you understand exceptions
 Mistake: exceptions
Most common mistake: you think you understand how exceptions work in Haskell.
Alternatively: you falsely assume you will always pay attention to how they work.
 Laziness + exceptions = hard
errOrOk <- try $
  return [1, 2, error "kaputt"]
case errOrOk of
  Left err -> print err
  Right x  -> print x
 Laziness + exceptions = hard
errOrOk <- try $
  return (throw MyException)
case errOrOk of
  Left err -> print err
  Right x  -> print x
 Async exceptions: very hard Does this function close the socket?
  foo :: (Socket -> IO a) -> IO a
  foo f = do
    s <- openSocket
    r <- try $ f s
    closeSocket s
    ...
 Async exceptions: very hard How about:
  foo :: (Socket -> IO a) -> IO a
  foo f = mask $ \restore -> do
    s <- openSocket
    r <- try $ restore $ f s
    closeSocket s
    ...
 Debugging async exceptions
1. Easy, this function shouldn’t throw exceptions.
2. The exception comes from another place? Let’s use mask?
3. Maybe we can set unsafeUnmask to allow an interrupt?
4. ...
5. Nothing works and programming is an eternal cycle of pain and darkness.
6. Gobackto1
 Exceptions: avoiding the madness
Keep your code pure where possible.
 Exceptions: avoiding the madness
Try to avoid throwing exceptions from pure code. If you have an exception lurking somewhere deep within a thunk, bad things will usually happen.
 Exceptions: avoiding the madness
Use more predictable monad stacks:
▶ IO (Either e a)
▶ Control.Monad.Except
 Exceptions: avoiding the madness
Use existing solutions such as resource-pool , resourcet , and existing patterns like bracket .
 Mistake: avoiding GHC extensions
 Mistake: avoiding GHC extensions
Before I wrote Haskell: C/C++ background. Lots of pain trying to get things compiling on MSVC++.
 Mistake: avoiding GHC extensions
1. There is only GHC
2. Extensions widely used in “standard” packages
3. It’s Haskell so refactoring is easy
 Mistake: avoiding GHC extensions
Average of around 3 extensions per module. Commonly: OverloadedStrings , TemplateHaskell , DeriveDataTypeable , ScopedTypeVariables , GeneralizedNewtypeDeriving , BangPatterns
 Mistake: avoiding GHC extensions
Extensions that improve readability are always a good idea!
LambdaCase , ViewPatterns , PatternGuards , MultiWayIf , TupleSections , BinaryLiterals
 Mistake: avoiding GHC extensions
Extensions that write code for you are even better!
DeriveFunctor , DeriveFoldable , DeriveTraversable , RecordWildCards , DeriveGeneric
 Mistake: avoiding GHC extensions Further reading:
Oliver Charles:
24 days of GHC extensions
 Mistake: not testing enough
 Mistake: not testing enough
If it compiles, ship it.
 Mistake: not testing enough QuickCheck/SmallCheck
  prop_revRev xs =
    reverse (reverse xs) == xs
 Mistake: not testing enough
Lots of tutorials available on QuickCheck/SmallCheck, but many people only use this to test a pure core of their application.
 Mistake: not testing enough Test your entire application!
▶ hspec-snap
▶ hspec-webdrivers ▶ tasty-golden
▶ ...
 Mistake: not testing enough Test your entire application!
▶ Roll your own framework
▶ Bash
▶ Python
▶ JavaScript
▶ ...
 Mistake: string types
 Mistake: string types
1. Write some code
2. It’s pretty slow
3. Find a stackoverflow thread from 2008 that tells me to use ByteString
4. Z rich
 ?
 Mistake: string types
  Binary or human-readable?
  Use ByteString
Are you writing hello world?
  Use String
Use Text
  Binary Human readable Yes No

 Mistake: string types
    Yes No
Yes No
No Yes
        Use Pipes/Conduit/...
Easily fits in memory?
Use Strict
Single pass over the data?
 Do you "get" unsafeInterleaveIO?
 Use Lazy I/O
Database? mmap?
 Mistake: not enough pure code
 Mistake: not enough pure code
  main :: IO ()
  main = do
    x <- readLn :: IO Int
    y <- readLn :: IO Int
    -- wizzling
    let wizzled =
x+y*y*3+9 print wizzled
 Pure code: sometimes it’s easy
wizzle :: Int -> Int -> Int
wizzle x y =
x+y*y*3+9
 Pure code: sometimes it’s easy
main :: IO ()
main = do
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  print $ wizzled x y
 Pure code: sometimes it’s easy
1. Read your input 2. Do your stuff
3. Write your output
 Pure code: sometimes it’s hard
▶ Can’t read all input because of memory constraints
▶ Input reading depends on temporary results
▶ Need for threads, continuous output
▶ ...
 Pure code: example
  data Request = Request
    { rqPath :: String
    , ...
    }
  data Response
    = Response200 String
    | Response404
 Pure code: example
  handler
    :: Request -> IO Response
  handler = do
    rq <- readRequest
    case rqPath rq of
      "/index" ->
        return $ Response200 "ok"
      _        ->
        return Response404
 Pure code: example
Startup idea: People can’t be expected to reverse their own strings if they are on a low-powered device like an IoT-enabled toaster. We should have a cloud service for reversing a string.
 Pure code: example
  data Request = Request
    { rqPath :: String
    , rqBody :: String
    , ...
}
  data Response
    = Response200 String
    | Response404
 Pure code: example
  handler = do
    rq <- readRequest
    case rqPath rq of
      "/rev"   ->
        return $ Response200 $
          reverse (rqBody rq)
      ...
 Pure code: example
  data Request = Request
    { rqPath :: String
    , ...
    }
  data Response
    = Response200 String
    | Response404
    | ReadBody
(String -> Response)
 Pure code: example
  handler = do
    rq <- readRequest
    case rqPath rq of
      "/rev"   -> return $
        ReadBody $ \body ->
          Response200 $
          reverse body
 Pure code: example
  run (Response200 str) =
    sendResponse str
  run Response404 =
    error "404"
  run (ReadBody f) = do
    body <- readRequestBody
    run (f body)
 Pure code: free
data Free (f :: * -> *) a
  = Free (f (Free f a))
  | Pure a
 Pure code: free
  data Response
    = Response200 String
    | Response404
  data HandlerF a
    = ReadBodyF (String -> a)
    deriving (Functor)
  type Handler =
      Free HandlerF
 Pure code: free
  getBody :: Handler String
  getBody = Free $
      ReadBodyF return
  ok200
    :: String
    -> Handler Response
  ok200 str = Pure $
      Response200 str
 Pure code: free
  handler
    :: Request
    -> Handler Response
  handler rq =
    case rqPath rq of
      "/rev" -> do
        str <- getBody
        ok200 $ reverse str
 Pure code: free
  run :: Handler Response -> IO ()
  run (Pure (Response200 str)) =
    sendResponse str
  run (Pure (Response404)) =
    error "404"
  run (Free (ReadBodyF f)) = do
    body <- readRequestBody
    run (f body)
 Mistake: overdoing abstraction
 Mistake: overdoing abstraction
  zygoHistoPrepro
    :: (Unfoldable t, Foldable t)
    => (Base t b -> b)
    -> (forall c. Base t c -> Base t c)
    -> (Base t (EnvT b (Stream (Base t))
-> a) -> t -> a
  zygoHistoPrepro f g t =
    -- unless you want a generalized
    -- zygomorphism.
    gprepro (distZygoT f distHisto) g t
 Mistake: overdoing abstraction
getProp k =
  case getProperty k m of
    Left  err -> fail err
    Right val ->
      return (show val)
 Mistake: overdoing abstraction Or do you prefer this version?
  getProp =
    either fail (return . show) .
    flip getProperty
 Mistake: Premature abstraction
 Mistake: premature abstractions Example: the resource provider in
Hakyll.
An abstaction around the file system to list and obtain resources as various types. It provides some caching and access to metadata.
 Mistake: premature abstractions
▶ Zero or one instances
▶ Lots of code working around restrictions
▶ Making assumptions about specific behaviours
▶ Too general
▶ Too abstract
 Mistake: premature abstractions (Historically incorrect) example:
  class Functor m => Monad m where
    return :: a -> m a
    join   :: m (m a) -> m a
    -- wat
    fail   :: String -> m a
 Mistake: premature abstractions
Abstractions are very hard to get right.
In general, just writing and repeating the non-abstract code until you really see the pattern helps.
 Questions?






