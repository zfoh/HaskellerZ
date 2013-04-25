% Cmdline libraries, intro to TemplateHaskell
% Gergely Risko (<gergely@risko.hu>, <s>errge@google.com</s>)
% HaskellerZ meetup - April 25th, 2013

Agenda 1
========

Demonstration of some command-line option parsing libraries:

  - cmdtheline

  - getopt

  - HFlags

    ~~~~ {.haskell}
    {-# LANGUAGE TemplateHaskell #-}

    import HFlags

    defineFlag "name" "Indiana Jones" "Who to greet"
    defineFlag "repeat" (3 + 4 :: Int) "Number of repeats"

    main = do s <- $(initHFlags "Simple program v0.1")
              sequence_ $ replicate flags_repeat $ putStrLn flags_name
    ~~~~

    That's all, after that you have `--version`, `--help` and `--undefok` for free.

Agenda 2
========

I'll also show some TemplateHaskell, a macro language to reduce boilerplate.

~~~~ {.haskell}
{-# LANGUAGE TemplateHaskell #-}

module Tup where

import Language.Haskell.TH

get n k = do
  name <- newName "wanted"
  let arg = tupP (replicate k wildP ++
                  [varP name] ++ replicate (n - k - 1) wildP)
  let body = varE name
  lamE [arg] body
~~~~

~~~~ {.haskell}
{-# LANGUAGE TemplateHaskell #-}

import Tup

main = print ($(get 6 3) (0,1,2,3,4,5))
~~~~

cmdtheline
==========

CmdTheLine uses applicative functors to provide a declarative,
compositional mechanism for defining command-line programs by lifting
regular Haskell functions over argument parsers.  What's the problem?

~~~~ {.haskell}
import Control.Applicative
import System.Console.CmdTheLine

name :: Term String
name = value $ opt "Indiana Jones" $ optInfo [ "name", "n" ]

times :: Term Int
times = value $ opt 4 $ optInfo [ "times", "t" ]

hello name times = sequence_ $ replicate times $ putStrLn name

term = hello <$> name <*> times

termInfo = defTI { termName = "Hello", version = "1.0" }

main = run ( term, termInfo )
~~~~

Monad -> Applicative -> Functor
===============================

Every Monad is an Applicative and every Applicative is a Functor, here is why:

~~~~ {.haskell}
fmap   :: Functor f     => (a -> b) -> f a -> f b
(>>=)  :: Monad m       => m a -> (a -> m b) -> m b
(<*>)  :: Applicative f => f (a -> b) -> f a -> f b
return :: Monad m       => a -> m a
pure   :: Applicative f => a -> f a

fmapFromAp :: Applicative f => (a -> b) -> f a -> f b
fmapFromAp pf av = (pure pf) <*> av

fmapFromM :: Monad m => (a -> b) -> m a -> m b
fmapFromM pf mv = mv >>= (\x -> return $ pf x)

apFromM :: Monad m => m (a -> b) -> m a -> m b
apFromM mf mv = mv >>= pvToRet
  where
    pvToRet pv = mf >>= pvpfToRet pv
    pvpfToRet pv pf = return (pf pv)

-- of course, fmapFromM was redundant
fmapFromM2 pf mv = (return pf) `apFromM` mv
~~~~

So, Monads are more specific than Applicatives, and in turn
Aplicatives are more specific than Functors.

Inside cmdtheline: why Term is an Applicative?
==============================================

The important data structure inside cmdtheline is the Term.

Why is that an Applicative?  Why not only Fuctor?  Why not Monad if
already an Applicative?

~~~~ {.haskell}
data Term a = Term [ArgInfo] (Yield a)
type Yield a = EvalInfo -> CmdLine -> Err a

instance Functor Term where
  fmap = yield . result . result . fmap
    where
    yield f (Term ais y) = Term ais (f y)
    result = (.)

instance Applicative Term where
  pure v = Term [] (\ _ _ -> return v)

  (Term args f) <*> (Term args' v) = Term (args ++ args') wrapped
    where
    wrapped ei cl = f ei cl <*> v ei cl
~~~~

It has to be an Applicative, so we can merge different parts of a
program that uses different arguments to a bigger program that uses
all of the arguments.  Functor is not enough, that only ever cares
about one context.

But it can't be a Monad, because Monads are too specific to be able to
evaluate a context before the computation.  So we won't be able to
(++) together all the args as we do in the Applicative instance.

Cmdtheline summary
==================

Pros:

  - Very clever idea

  - Clean and readable implementation

  - Good documentation with lot of examples

  - Lot of features: e.g. man page generation

Cons:

  - The approach needed in Main.hs is a bit cryptic for a beginner

  - Tutorial website is down and not included on GitHub

  - Composition with options in libraries is cumbersome

GetOpt
======

Similar to C getopt, included in base.

~~~~ {.haskell}
import System.Console.GetOpt
import System.Environment

data MyOptions =
  MyOptions { name :: String
            , times :: Int }

def = MyOptions { name = "Indiana Jones", times = 4 }

options =
  [ Option ['n'] ["name"]
    (ReqArg (\name r -> r { name = name }) "NAME") "Who to greet"
  , Option ['t'] ["times"]
    (ReqArg (\times r -> r { times = read times}) "INT") "How many times" ]

run opts = sequence_ $ replicate (times opts) $ putStrLn (name opts)

main = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o,n,[]  ) -> run (foldr ($) def o)
    (_,_,errs) -> ioError (userError $ concat errs ++ usageInfo "example" options)
~~~~

GetOpt summary
==============

Pros:

  - Simple, by the book solution

  - Clean control flow

  - Included in base (so comes with GHC, you don't even need the platform)

Cons:

  - Even the simple thing is long

  - Had to roll our own foldr ($) def to handle default args and unordered opts

  - Composition with options in libraries gonna be a nightmare

HFlags uses GetOpt for parsing.

HFlags
======

~~~~ {.haskell}
{-# LANGUAGE TemplateHaskell #-}

import HFlags

defineFlag "name" "Indiana Jones" "Who to greet"
defineFlag "repeat" (3 + 4 :: Int) "Number of repeats"

main = do s <- $(initHFlags "Simple program v0.1")
          sequence_ $ replicate flags_repeat $ putStrLn flags_name
~~~~

Pros:

  - Simple definition of flags and pure access to them

  - Very easy and nice API free of boilerplate

  - Support for `--undefok` (needed for complex deployments!), `--help`

  - Libraries can introduce flags on their own, no things to pass around

Cons:

  - Impure, people will yell at me

  - Uses TemplateHaskell to have the nice interface (portability issues)

  - main has to splice in the initialization function with TH

Demo time!
==========

- SimpleExample

- ImportExample with collision detection

- Package example with cabal

- Complex example

Template Haskell Demo Environment
=================================

Template Haskell is easy experiment with inside GHCi, ideal for learning and tinkering.

- :set -XTemplateHaskell

- useful: pprintQ q = putStrLn . pprint =<< runQ q

- GHC flag for watching splices getting built: :set -ddump-splices

- work through intro.ihs

- work through reify.ihs

- work through HFlags.hs

Thank you
=========

Questions, remarks?

Template Haskell in the haskellwiki: [http://www.haskell.org/haskellwiki/Template_Haskell](http://www.haskell.org/haskellwiki/Template_Haskell)

Paper on TemplateHaskell: [http://research.microsoft.com/~simonpj/papers/meta-haskell/](http://research.microsoft.com/~simonpj/papers/meta-haskell/)

Design notes for V2: [http://research.microsoft.com/en-us/um/people/simonpj/tmp/notes2.ps](http://research.microsoft.com/en-us/um/people/simonpj/tmp/notes2.ps)

Reference docs on Hackage: [http://hackage.haskell.org/package/template-haskell](http://hackage.haskell.org/package/template-haskell)

HFlags on Hackage: [http://hackage.haskell.org/package/hflags](http://hackage.haskell.org/package/hflags)
