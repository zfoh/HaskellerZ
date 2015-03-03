- Author: Simon Meier <iridcode@gmail>
- Date: 2015-02-28

# The Service Pattern

This tutorial describes a design pattern
  that I discovered when working at Better.
It explains how to structure
  the `IO`-layer of a Haskell application as a
  [Service-oriented architecture](http://en.wikipedia.org/wiki/Service-oriented_architecture).
By `IO`-layer,
  I mean the set of all functions that can execute `IO` actions.
At Better,
  we used this pattern extensively
  to modularize the `IO`-layer of our application.

Acknowledgements: thanks to Jasper Van der Jeugt for proof reading.


## When to use the service pattern

The service pattern allows you to modularize the use of subsystems
  that are shared between different parts of the `IO`-layer of your
  application.
Typical examples of such subsystems are
  logging and monitoring support,
  database access,
  caches,
  authentication and authorization support,
  or
  connection pools.
We found that it is easier to refer to these sub-systems as *services*,
  which is what we are going to use in the remainder of this tutorial.


## What is the service pattern

The service pattern defines a particular module structure for implementing
  services.
Before giving a formal definition of this structure,
  I'm going to demonstrate it on a simple example.

### A simple logging service

Logging is a central functionality in many applications.
It is a must for web-application servers,
  and
  it provides a principled means to implement console output at different
  verbosity levels for command-line tools.
Interestingly,
  logging is a functionality for which most applications require at least two
  different implementations.
During execution all code should log to the same file or socket
  according to the verbosity level,
    while during testing we would like each test to have its own logger
    and we would like to verify that no warnings or errors are logged.
We can achieve this polymorphicity as follows.
We first introduce an abstract interface for a logger.

``` haskell
-- | This is an abstract interface for a simple logger. It is intended to be
-- imported qualified as follows.
--
-- > import qualified Acme.System.Logger as Logger
--
module Acme.System.Logger
  (
    -- * Abstract handles
    Handle(..)

    -- * Priorities and convenience functions
  , Priority(..)

    -- | The following functions simplify logging at a fixed priority level.
  , logDebug
  , logInfo
  , logWarning
  , logError

  ) where

import qualified Data.Text as T

data Priority
    = Debug    -- ^ Debug messages
    | Info     -- ^ Notable information that requires no immediate action.
    | Warning  -- ^ Something is probably wrong, and we should investigate.
    | Error    -- ^ Something is wrong and immediate action is required.
    deriving (Eq, Ord, Show)

newtype Handle = Handle
    { log :: Priority -> T.Text -> IO () }

logDebug, logInfo, logWarning, logError :: Handle -> T.Text -> IO ()
logDebug   = (`log` Debug)
logInfo    = (`log` Info)
logWarning = (`log` Warning)
logError   = (`log` Error)
```

Functions that require access to logging can now be written against this
  abstract interface.
For example,
  a naive web-crawler could have the signature.
``` haskell
import qualified Acme.System.Logger as Logger

collectLinksBelow :: Logger.Handle -> T.Text -> IO [T.Text]
```

We can write concrete implementations of this logger are as follows.

``` haskell
-- | A logger implementation that logs all messages to a 'System.IO.Handle'.
module Acme.System.Logger.Impl.FileHandle
  ( newHandle
  ) where

import qualified Acme.System.Logger      as Logger
import           Data.Monoid             (<>)
import           Control.Concurrent.MVar (newMVar, withMVar)
import qualified System.IO

-- | Create a new 'Logger.Handle' that logs to a 'System.IO.Handle'.
newHandle :: System.IO.Handle -> IO Logger.Handle
newHandle fileH = do
    -- We use a mutex to make our logger thread-safe.
    -- (Note that we should take this mutex as an argument for maximal
    -- compositionality.)
    mutex <- newMVar ()

    return $ Logger.Handle
      { Logger.log = \prio msg ->
            withMVar mutex $ \() ->
                hPutStrLn fileH $ show prio <> ": " <> T.unpack msg
      }
```

``` haskell
-- | A logger implementation that stores all messages in an `IORef` for latter
-- inspection by a test-harness.
module Acme.System.Logger.Impl.Pure
  ( withHandle
  ) where

import qualified Acme.System.Logger as Logger

import           Data.IORef (newIORef, atomicModifyIORef, readIORef)


-- | Create a 'Logger.Handle' records all messages and returns them once the
-- inner 'IO' action terminates.
withHandle :: (Logger.Handle -> IO a) -> (a, [(Priority, T.Text)])
withHandle io = do
    -- We use a shared 'IORef' to records all log messages in reverse order.
    msgsRef <- newIORef []
    -- Call inner IO action
    x <- io $ Handle
      { Logger.log = \prio msg ->
            -- We don't need the strict variant here, as we are just
            -- accumulating constructors.
            atomicModifyIORef msgsRef $ \msgs -> ((prio, msg) : msgs, ())
      }
    -- return the logged messages
    msgs <- readIORef msgsRef
    return (x, reverse msgs)
```


### Formal definition of the service pattern

The service pattern is built around the two concepts of
  service specifications and service implementations.
A *service specification* for a service `X` is a module of the following form.
``` haskell
-- | Specification of the X service. This module is intended to be imported
-- qualified as follows.
--
-- > import qualified YourModulePrefix.X as X
--
module YourModulePrefix.X
  (
    -- * Abstract handle
    Handle(..)

    -- * Pure types
    -- Here you export types that are necessary to interact with service X,
    -- but are too specific to fit into their own module, e.g., the priorities
    -- above in the logger service.

    -- * Derived functions
    -- Here you export derived functions that work for any implementation of
    -- service X.
    -- They are typically of the shape `fun :: Handle -> params -> IO result`

  ) where

data Handle = Handle
    { function1 :: !(param1_1 -> ... -> param1_N1 -> IO result1)
      ...
    , functionM :: !(paramM_1 -> ... -> paramM_NM -> IO resultM)
    }
```
Note that this is analogous to manually constructing type-class
  dictionaries.
See the Section "Discussion of the service pattern"
  for a discussion on why we
  prefer this formulation to one based on type-classes.

A *service implementation* for a service `X`
  is a module of the following form.
``` haskell
-- | Implementation of the X service that uses ...
module YourModulePrefix.X.Impl.DescriptiveImplName
  ( Config(..)
  , withHandle
  ) where

-- Import specifications for services that we require for this implementation.
import qualified YourModulePrefix.Service1 as Service1
...
import qualified YourModulePrefix.ServiceN as ServiceN

-- Import specification that we are implementing.
import qualified YourModulePrefix.X        as X


-- | Pure representation of the configuration of your service implementation.
-- This type provides both named parameters for the 'withHandle' function and
-- a representation that can be easily serialized or parsed from an external
-- config file.
data Config = Config
    { cParam1 :: !t1
      ...
    , cParamM :: !tM
    }
    deriving (Eq, Show)

-- | Run an 'IO' action with access to an 'X.Handle'.
withHandle
    :: Config             -- ^ Configuration
    -> Service1.Handle    -- ^ Service dependency 1 (e.g. a logger)
    -> ...
    -> ServiceN.Handle    -- ^ Service dependency N
    -> (X.Handle -> IO a)
    -> IO a
withHandle config reqService1H ... reqServiceNH = do
    -- allocate implementation resources (e.g., a mutex)
    -- construct 'X.Handle'
    -- run inner action while making sure that we deallocate our resources
    --   both on exceptions and normal termination

-- For complicated services it often helps to define an implementation handle
-- as follows. It might even be that you have whole module structure below
-- 'YourModulePrefix.X.Impl.DescriptiveImplName' that you use to implement
-- service 'X'.
data IHandle = IHandle
    { ihConfig        :: !Config
    , ihService1      :: !Service1.Handle
      ...
    , ihServiceN      :: !ServiceN.Handle
    , ihImplResource1 :: !t1
      ...
    , ihImplResource1 :: !tM
    }

-- | Implementation of 'X.Handle.function1'.
function1 :: IHandle -> param1_1 -> ... -> param1_N -> IO result1

-- ... remaining service function implementations ...
```
These two definitions are quite long.
To understand the definitions,
  it helps to compare them to the logger example given earlier.
I also invite you to ponder how you could apply this pattern in your codebase.
Sadly, I cannot point to the two codebases where I used this pattern,
  as both of them are closed-source.


## Discussion of the service pattern

The key benefits gained from the service pattern are
  modularity and increased compositionality.
We can use this pattern to structure our application purely
  as a composition of services.
In fact the main function of the Better web-application,
  looks roughly as follows.
``` haskell
import Better.Http.Impl.Snap             as Http.Impl.Snap

import Better.Storage.Impl.Postgres      as Storage.Impl.Postgres

import Better.System.Monitor.Impl.Ekg    as Monitor.Impl.Ekg
import Better.System.Logger.Impl.Default as Logger.Impl.Default

-- NOTE that Postgres and LRUCache are services where we combine the
-- specification and the implementation, as we currently have only one
-- implementation. We can easily abstract later, once we need additional
-- implementations.
import Better.System.Postgres            as Postgres
import Better.System.LRUCache            as LRUCache

-- We use https://hackage.haskell.org/package/managed to avoid the
-- right-indent drift from the 'withXXX' functions.
import Control.Monad.Managed (runManaged, managed)

-- | Start the Better web-service.
main :: IO ()
main = do
    -- get name of config file from arguments
    configFile <- parseArguments

    -- parse service implementation configurations from the file
    monitorC  <- parseMonitorC configFile
    loggerC   <- parseLoggerC  configFile
    postgresC <- parsePostresC configFile
    -- ... more config parsing

    -- construct service handles and run until the process is killed
    runManged $ do
        monitorH  <- managed $ Monitor.Impl.Ekg.withHandle monitorC
        loggerH   <- managed $ Logger.Impl.Default.withHandle loggerC monitorH
        postgresH <- managed $ Postgres.withHandle postgresC monitorH loggerH
        lruCacheH <- managed $ LRUCache.withHandle lruCacheC monitorH

        -- Our storage service provides all functionality related to
        -- persistence. Its API is quite large. Our production implementation
        -- of this service uses PostgreSQL.
        storageH  <- managed $ Storage.Impl.Postgres storageC lruCacheC loggerH postgresH

        -- ... more services for implementing our business logic and in the end
        -- ... our snap handlers

        snapH <- managed $ Http.Impl.Snap.withHandle snapC monitorH loggerH handlers

        -- wait until the the process is killed
        forever $ threadDelay 100000
```

Note that with the service pattern we can push all implementation decisions
  to the `main` function, i.e., the root of our dependency tree.
All service implementations themselves can be implemented completely
  independent of each other.
Moreover,
  services implementations can make all their dependencies explicit,
    which enables us to mock them appropriately during testing.
It also means that more of our code becomes platform independent,
  as we can switch dependencies easily.

In my opinion,
  there are two non-obvious design decisions in the above definition of
  the service pattern.
First,
  it does not abstract over the concrete monad, but uses `IO` pervasively.
Second,
  it does not use type-classes to specify abstract `Handle`s.
We discuss the rationale for these two decision in the following two sections.


### Relying on just IO

The main reason why the functions in a service specification just use
  `IO result` instead of custom monad stacks is that this composes better.
Monad stacks fix the order of arguments and produce friction
  at abstraction boundaries due to the necessary transformations between
  different monad stacks.
We found it to be more pleasant to just pass around the handles to the
  necessary services as parameters (possibly grouped in a record).
This way all dependencies are explicit
  and function application is all we need pass around handles.

On a related note,
  we found at Better that adding effects to `IO` is often unnecessary,
  as `IO` is powerful enough on its own.
The typical use was just a `MaybeT` or `EitherT` to track exceptions
  explicitly in the types.
On service boundaries,
  we typically translated these exceptions to a descriptive log-message and
  a simple `Nothing`.

There can be made a case for abstracting `Handle`s over the the monad used to
  compute results, i.e., as
  `data Handle m = Handle { fun1 :: ... -> m result }`.
This can be useful for building pure models of an application for testing.
We found that is often sufficient to build semi-pure implementations
  that are based on hidden `IORef`s.


### Why not use type classes to define handles?

The short answer is that we do not use type classes because
  we would neither gain further functionality nor code that is easier to
  reason about.
The long answer is the following.

We see two options on how one can use type classes to manage service
  dependencies.
The first one is to attach service specifications to the monad
  in which we compute the results.
For example,
  we could define the following two type classes to model monads
  that support logging and database access.
``` haskell
class MonadLogger m where
    log :: Priority -> T.Text -> m ()

class MonadDB m  where
    runTransaction :: Transaction a -> m a
```
The service dependencies of any function are then captured by corresponding
  type class constraints, as shown in the following example.
``` haskell
someBusinessLogic
    :: (MonadLogger m, MonadDB m)
    => param1 -> ... -> paramN -> m result
someBusinessLogic ... = do
    mbResult <- runTransaction someTransaction
    case mbResult of
      Just result -> return result
      Nothing     -> do
        log Warning "falling back to default"
        return defaultResult
```
The main problem that we see with this approach is that it strongly
  favors the situation where all service dependencies are satisfied by a
  single instance.
This situation is however rather seldom,
  as for example configuring using different
  loggers for different service instances is common (e.g., to set different
  verbosity levels).
One can handle this case using mtl-style transformer stacks,
  but this requires more effort compared to just passing the handles
  explicitly.
Moreover,
  the explicit handles also work in the case where we need to
  abstract over some pure functions,
  e.g., how to render a link to an HTTP API call on the backend.
In this case,
  the monad typeclasses introduce unnecessary sequencing.

The second option to use type classes is to use
  functions of the form
  `fun1 :: X.Handle h => h -> param1 -> ... -> paramN -> IO result`.
This would allow a single handle to implement multiple services.
However,
  we are not sure where this would be beneficial.
A service's dependencies should always be kept private;
  and in a service implementation we can just store the handles of service
  dependencies as fields in the implementation handle.
In general,
  we found that being explicit about dependencies simplified reasoning about
  our service implementations and did not negatively impact refactoring.

### How does this relate to object-oriented programming?

The implementation strategy used in the service pattern corresponds closely to
  how interfaces and classes work in OOP.
Our service specifications declare both an interface at the type level,
  and a value representation for the interface at runtime.
In terms of memory consumption,
  our interfaces are heavier than typical class instances in an OOP language.
We store at least one pointer to the instance-data per closure,
  whereas OOP implementations
  typically store a single pointer to the
  [virtual method table](http://en.wikipedia.org/wiki/Virtual_method_table)
  together with the other instance-data.
In terms of call overhead,
  we pay the cost of an
  [unknown function call](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls#Genericapply),
    which is probably a bit slower than a virtual method invocation in an OOP
    langauge like Java.
If this becomes a performance bottleneck,
  we will have to avoid the abstraction and specialize at compile time.
[Backpack](https://ghc.haskell.org/trac/ghc/wiki/Backpack)
  will allow us to do this in a principled fashion without losing modularity.
Compared to OOP languages,
  our interfaces are more powerful,
    as they have a first order representation.
Their functions can be mixed and matched freely,
  and we are not constrained to explain extension via inheritance.


## Conclusion

This tutorial documents a pattern that I found
  to apply pervasively in the codebases I worked on.
The description of the pattern is a bit long,
  but I invite you to give it a shot in your codebase.
I am looking forward to hearing how it worked out.
Happy coding :-)
