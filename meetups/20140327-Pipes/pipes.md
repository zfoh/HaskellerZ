% Programming with `pipes` and some implications of the Free monad-style monadic programming
% Mihály Bárász, nilcons.com
% HaskellerZ meetup - March 27th, 2014

Outline
=======

  - Intro to pipes

  - View of pipes: structured continuation passing

  - The `Proxy` type

  - Brief overview of pipes ecosystem

<!--  - Monad morphisms -->

  - Analysis of two deep "bugs"


Trivial example
=====
~~~ haskell
main = runEffect $ streamList [0..] >-> take 10 >-> printAll

streamList xs = forM_ xs yield

take 0 = return ()
take n = do
  x <- await
  yield x
  take (n-1)

printAll = forever $ do
  x <- await
  lift $ print x
~~~

Trivial example (with types)
=====
~~~ haskell
import Control.Monad (forever, forM_)
import Pipes
import Prelude hiding (take)

main = runEffect $ streamList [0..] >-> take 10 >-> printAll

streamList :: [a] -> Producer a IO ()
streamList xs = forM_ xs yield

take :: Int -> Pipe a a IO ()
take 0 = return ()
take n = do
  x <- await
  yield x
  take (n-1)

printAll :: Show a => Consumer a IO ()
printAll = forever $ do
  x <- await
  lift $ print x
~~~

Trivial example (with better types)
=====
~~~ haskell
import Control.Monad (forever, forM_)
import Pipes
import Prelude hiding (take)

main = runEffect $ streamList [0..] >-> take 10 >-> printAll

streamList :: Monad m => [a] -> Producer a m ()
streamList xs = forM_ xs yield

take :: Monad m => Int -> Pipe a a m ()
take 0 = return ()
take n = do
  x <- await
  yield x
  take (n-1)

printAll :: Show a => Consumer a IO r
printAll = forever $ do
  x <- await
  lift $ print x
~~~

Examples from real code
=======================

~~~ haskell
forkIO $ runEffect $
  measureCPU >-> delay >-> collectN 100 >-> updateIcon cpuIcon blue1 blue2
~~~

Examples from real code #2
==========================

~~~ haskell
runEffect $ readChanSignalledS (rawTWSInput raw)
        >-> useD (\t -> $logDebug $ printf "New incoming message: %s" (show $ twsI t))
        >-> timePipe
        >-> nextIdPipe
        >-> managedAccountPipe
        >-> errMsgPipe
        >-> sanitizeCashOrdersInPipe
        >-> filledOrdersInPipe
        >-> openOrdersInPipe
        >-> writeChanD chanFromTWS_
~~~

View pipes as coroutines
========================

. . .

~~~ haskell
data Pipe a m r = Pipe { stepPipe :: m (Step a m r) }

data Step a m r = Yield (a, Pipe a m r)
                | Await (a -> Pipe a m r)
                | Done r
~~~

An excellent discussion in
[Monad.Reader #19: Mario Blažević, Coroutine Pipelines](http://themonadreader.files.wordpress.com/2011/10/issue19.pdf)

The real `Proxy` type
=====================

~~~ haskell
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
~~~

. . .

A `Proxy` is a monad transformer that receives and sends information
on both an upstream and downstream interface.

  The type variables signify:

  * `a'` and `a` - The upstream interface, where `(a')`s go out and `(a)`s
    come in

  * `b'` and `b` - The downstream interface, where `(b)`s go out and `(b')`s
    come in

  * `m` - The base monad

  * `r` - The return value

The `Proxy` type diagrammatically
=====================

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/proxy.svg)

<br/>

~~~ haskell
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
~~~

Composing Proxies
=====================

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/proxy_cat.svg)

<br/>

~~~ haskell
(>->) :: Monad m => Proxy a' a () b m r -> Proxy () b c' c m r -> Proxy a' a c' c m r
~~~

`Proxy` type aliases
====================

`Producer`s can only `yield`:

~~~ haskell
type Producer b = Proxy X () () b
type Producer' b m r = forall x' x. Proxy x' x () b m r
~~~

`Consumer`s can only `await`:

~~~ haskell
type Consumer a = Proxy () a () X
type Consumer' a m r = forall y' y. Proxy () a y' y m r
~~~

Here `X` is an uninhabited type (like `Data.Void`).

Alternative ways to compose Proxies
===================================

There are 5 predefined ways of composing pipes (or 4, or 7, depends
how you look at it). And they can also be intermixed freely. These
define the following categories over `Proxy`:

 * The `cat` or pipes category: `(>->)` and `cat`.
 * The `pull` category: `(>+>)` and `pull`.
 * The `push` category: `(>~>)` and `push`.
 * The `request` category: `(\>\)` and `request`.
 * The `respond` category: `(/>/)` and `respond`.
 * `(~>)` and `yield`.
 * `(>~)` and `await`.

Understanding composition
=========================

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/proxy_input.svg)

<br/>

~~~ haskell
i -> Proxy a' a b' b m r
~~~

Understanding composition
=========================

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/proxy_respond.svg)

<br/>

~~~ haskell
respond :: b -> Proxy a' a b' b m b'
~~~

Understanding composition
=========================

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/proxy_resp_cat.svg)

<br/>

~~~ haskell
(/>/) :: Monad m
  => (a -> Proxy x' x b' b m a')
  -> (b -> Proxy x' x c' c m b')
  ->  a -> Proxy x' x c' c m a'
~~~

Understanding composition
=========================

Pipes composition is not limited to the ways provided by `pipes`
package. If you are willing to "look inside" (by using the
`Pipes.Internal` module).

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/proxy_loop.svg)

<br/>

~~~ haskell
loopPipe :: Monad m => Maybe a -> Proxy () a () a m r -> Effect' m r
~~~

The Pipes ecosystem
===================

Pipesification of specific libraries, libs with pipes API:

 * pipes-bytestring
 * pipes-text  (not yet)
 * pipes-binary
 * pipes-aeson
 * pipes-attoparsec
 * pipes-zlib
 * pipes-network, pipes-network-tls
 * pipes-http
 * pipes-csv
 * pipes-postgresql-simple
 * pipes-shell

Not really pipes specific, but designed for pipes

 * pipes-concurrency
 * pipes-safe

Extending pipes functionality

 * pipes-parse
 * pipes-group

"Leaking" `Producer`s
=====================

In (surprisingly) many ways pipes -- especially `Producer`s -- behave
like lists.

Can you spot the problem?

~~~ haskell
numbers :: Producer Int IO r
numbers = go 0
  where
    go n = do yield n
              go (n+1)
~~~

"Leaking" `Producer`
=====================

The problem is that you can't use the `numbers` twice in your
code. The following program leaks:

~~~ haskell
import Pipes
import Pipes.Core

numbers :: Producer Int IO ()
numbers = go 0
  where
    go n = do yield n
              go (n+1)

main :: IO ()
main = do
  runEffect $ numbers //> lift . print
  runEffect $ numbers //> lift . print
~~~

. . .

~~~ haskell
import Control.Monad

numbers :: [Int]
numbers = go 0
  where
    go n = n : go (n+1)

main :: IO ()
main = do
  forM_ numbers print
  forM_ numbers print
~~~

"Leaking" graphically
=====================

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/numbers_struct.svg)

<br/>

. . .

This is a bit misleading, the real question is what thunks are created
and what have reference to what. You can visualise this with ghc-vis.

How to prevent this from happening
==================================

Sometimes an optimized build might help. Eg. we can produce the same
kind of structure simply in `IO`, no pipes involved:

~~~ haskell
printNumbers :: IO ()
printNumbers = go 0
  where
    go n = do print n
              go (n+1)

main :: IO ()
main = do
  printNumbers
  printNumbers
~~~

And it does leak if compiled with `-O0`. But if compiled with `-O` it
runs in constant space.

How to prevent list-like leaks
==============================

Generally, _avoid defining recursive structures that don't depend on
anything (parameters or IO) and using them more than once_.

. . .

There's no silver bullet, though. Consider this:

~~~ haskell
numbersFrom :: Int -> Producer Int IO ()
numbersFrom = go
  where
    go n = do yield n
              go (n+1)

main :: IO ()
main = do
  let n = 42
  runEffect $ numbersFrom n //> lift . print
  runEffect $ numbersFrom n //> lift . print
~~~

Solution: `-fno-full-laziness`.

Quadratic "appending"
=====================

Can you spot the issue?

~~~ haskell
chunkify :: Monad m => Int -> Pipe a [a] m r
chunkify n = forever $ do
  xs <- replicateM n await
  yield xs

main :: IO ()
main = do
  runEffect $ numbers >-> chunkify 10000 >-> P.take 1 //> lift . print . last
~~~

Explanation of quadratic behavior
=================================

To understand the issue we need to know two things:

 * The definition of `replicateM`:

~~~ haskell
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n op = go n
  where
    go 0 = return []
    go k = do
      x <- op
      xs <- go (k-1)
      return (x : xs)
~~~

 * Understanding why the following is quadratic:

~~~ haskell
numList :: Int -> [Int]
numList 0 = []
numList n = numList (n-1) ++ [n]
~~~

Explanation, graphically
========================

<br/>

\ \ \ \ \ \ \ \ \ \ \ \ ![](img/list.svg) \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ![](img/replicateM.svg)

<br/>

Quadratic behavior, analysis
============================

This doesn't happen with most commonly used monads, like `IO`,
`StateT`, `ReaderT`. How come?

But, this always happens with monads that represent structure of
computation explicitly, like `Proxy`, `ConduitT`, `Free`,
`operational` `Program` etc.

<!-- Consider: to evaluate an expression is
 such a monad, you need to find the left-most element in the bind tree;
 combine this with that bind have to explicitly traverse the structure,
 and you get that if the bind tree is not right-leaning then you are in
 trouble. -->

Solutions?

 * You can use an accumulator based `replicateM`, which is right-leaning:

~~~ haskell
accReplicateM :: Monad m => Int -> m a -> m [a]
accReplicateM n op = go n []
  where
    go 0 acc = return $ reverse acc
    go k acc = do
      x <- op
      go (k-1) (x : acc)
~~~

 * You can use `Data.Sequence`'s `replicateM`. (And get a $O(log^2 n)$
   behavior, instead of $O(n^2)$ one.)

. . .

 * Or, you can use magic.

CPS magic
=========

The magic is the same as for the list case: continuations.

The big gun: `Codensity` monad from `kan-extensions`.

~~~ haskell
chunkify :: Monad m => Int -> Pipe a [a] m r
chunkify n = forever $ do
  xs <- lowerCodensity $ replicateM n (lift await)
  yield xs
~~~

The universal solution: Church-encoding your data structure.

Thank you!
==========

Off topic: if you work a lot with date-time values in Haskell and are
annoyed with the `time` library, I'd like to hear from you!

For correct and efficient (and pure) handling of time zones check out
[github.com/nilcons/haskell-tz](https://github.com/nilcons/haskell-tz) (will release it to Hackage in
the next few days).

Monad morphisms
===============

~~~ haskell
hoist :: (MFuctor t, Monad m) => (forall a. m a -> n a) -> t m b -> t n b
~~~

With this you can use a rigidly typed pipe in a different pipeline.

~~~ haskell
import Control.Monad.Trans.State
import Pipes.Lift

pipeInIO :: Pipe a a IO ()

pipeInStateIO :: Pipe a a (StateT s IO) ()
pipeInStateIO = hoist lift pipeInIO
~~~

Monad morphisms to rearrange/remove layers
==========================================

~~~ haskell
evalStateP :: Monad m => s -> Proxy a' a b' b (StateT s m) r -> Proxy a' a b' b m r
~~~

. . .

~~~ haskell
distribute :: (Monad m, MonadTrans t, MFunctor t, Monad (t m), Monad (t (Proxy a' a b' b m)))
  => Proxy a' a b' b (t m) r
  -> t (Proxy a' a b' b m) r
distribute p =  runEffect $ request' >\\ hoist (hoist lift) p //> respond'
  where
    request' = lift . lift . request
    respond' = lift . lift . respond

runStateP :: Monad m
    => s
    -> Proxy a' a b' b (S.StateT s m) r
    -> Proxy a' a b' b m (r, s)
runStateP s p = (`S.runStateT` s) $ distribute p
~~~
