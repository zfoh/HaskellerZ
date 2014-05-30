% Make you a Monad with your favorite monad transformer library
% Ivan Jovanovic <ivan.jovanovic@gmail.com>
% HaskellerZ meetup - 29.05.2014

Outline
=======

 - Monads and additional effects of computations
 - Combination of multiple effects into one monad
 - Libraries to assemble and manage your monad stack
 - transformers
 - mtl
 - Real world example (X a)
 - operational

The thousand-and-first Monad tutorial
==================================
  - Programmable ";".
  - Allows us to control what happens between the two computations.

~~~~~~~~~~~~~~~~{.haskell}
class  Monad m  where
    -- | Inject a value into the monadic type.
    return      :: a -> m a
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: m a -> (a -> m b) -> m b
~~~~~~~~~~~~~~~~

  - Monadic laws.

~~~~~~~~~~~~~~~{.haskell}
    return a >>= k  ==  k a
    m >>= return  ==  m
    m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
~~~~~~~~~~~~~~~

  - composition
  - additional effects

Monadic composition
===================

  - Chaining two computations.
  - Explicitly naming the intermediate result.
  - Rest of the computation can use the intermediate result.

Monadic effects
===============

  - Possibility of failure.

~~~~~~~~~~~{.haskell}
instance (Error e) => Monad (Either e) where
    return        = Right
    Left  l >>= _ = Left l
    Right r >>= k = k r
~~~~~~~~~~~~

  - State threading.

~~~~~~~~~~~{.haskell}
instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= k  = State $ \s -> do
        (a, s') <- (runState m) s
        runState (k a) s'
~~~~~~~~~~~

  - Read-only context.

~~~~~~~~~~~{.haskell}
instance Monad (Reader r) where
    return   = lift . return
    m >>= k  = Reader $ \ r -> do
        a <- (runReader m) r
        runReader (k a) r
~~~~~~~~~~~

  - Write-only log.

~~~~~~~~~{.haskell}
instance (Monoid w) => Monad (Writer w) where
    return a = Writer (a, mempty)
    m >>= k  = Writer $ do
        (a, w)  <- runWriter m
        (b, w') <- runWriter (k a)
        return (b, w `mappend` w')
~~~~~~~~~

Combination of effects
=====================================

  - Threading state with the possibility of failure.

~~~~~~~~~~~~~~~~~~{.haskell}
instance Monad (StateWithError s e) where

    return a = StateWithError $ \s -> Right (a, s)

    (StateWithError p) >>= k =
            StateWithError $ \s0 ->
              case p s0 of
                Right (val, s1) ->
                  let (StateWithError q) = k val
                  in q s1
                Left e -> Left e
~~~~~~~~~~~~~~~~~~

Challenges of the composition
=============================
  - Monads are not straightforwardly compositional, we have to manage this composition.
  - "Functional Programming with Overloading and Higher-Order Polymorphism" - [http://web.cecs.pdx.edu/~mpj/pubs/springschool.html](http://web.cecs.pdx.edu/~mpj/pubs/springschool.html )
  - If we want to stay compositional we have to produce a monad.

Haskell libraries for monad stack management
============================================

  - Some history embedded into the current state.
  - Several ideas and approaches.
  - Separation proposal: [http://www.haskell.org/pipermail/libraries/2010-September/014281.html](http://www.haskell.org/pipermail/libraries/2010-September/014281.html )

~~~~~~~~~~~~~~~
* transformers is a Haskell 98 package containing
    * base functors (Data.Functor.Constant and Data.Functor.Identity),
    * operations on functors (Data.Functor.Compose and Data.Functor.Product),
    * transformer classes (Control.Monad.Trans.Class and
      Control.Monad.IO.Class) and
    * concrete monad transformers with code to lift operators
      (Control.Monad.Trans.*).
  The package can be used on its own (see the Control.Monad.Trans.Class
  documentation for examples), or with packages adding type classes.

* mtl-2 (the current monads-fd) depends on transformers and adds type
  classes using functional dependencies.  It has the same modules as
  mtl-1 and usage is very close, except for the differences listed below.
~~~~~~~~~~~~~~~

  - Two use cases
    - Using existing monads.
    - Defining your own monadic type.

transformers
============

  - lets start with a simple example of a computation in IO monad.

~~~~~~~~~~~~~{.haskell}
-- | Counts number of entries in each folder as it traverses the folder tree.
--   Stores the results in a list.
countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
    let newName = path </> name
    isDir <- doesDirectoryExist newName
    if isDir
      then countEntries newName
      else return []
  return $ (path, length contents) : concat rest
~~~~~~~~~~~~~

transformers - example using the WriterT transformer type
======================

  - Transformer types (WriterT, runWriterT)
  - Effect basis functions (tell)
  - Specific treatment of IO actions, liftIO

~~~~~~~~~~~~{.haskell}
-- | Traverses the folder tree and counts number of entries.
--   Writes results into a writer log along the way (tell).
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    -- store the value in the log
    tell [(path, length contents)]
    forM_ contents $ \name -> do
      let newName = path </> name
      isDir <- liftIO . doesDirectoryExist $ newName
      when isDir $ countEntries newName
~~~~~~~~~~~~

transformers - transformer types
========================

  - Type definition

~~~~~~~~~~~{.haskell}
-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
~~~~~~~~~~~~

  - It is still a monad.

~~~~~~~~~~~~{.haskell}
instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = writer (a, mempty)
    m >>= k  = WriterT $ do
        (a, w)  <- runWriterT m
        (b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
~~~~~~~~~~~~

  - Being a transformer means that it implements how underlying monads are lifted.

~~~~~~~~~~~~{.haskell}
instance (Monoid w) => MonadTrans (WriterT w) where
    lift m = WriterT $ do
        a <- m
        return (a, mempty)
~~~~~~~~~~~~

transformers - MonadIO type class, IO monad
=======================

  - MonadIO type class

~~~~~~~~~~~~{.haskell}
class (Monad m) => MonadIO m where
    -- | Lift a computation from the 'IO' monad.
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id
~~~~~~~~~~~~

  - And the WriterT instance

~~~~~~~~~~~~{.haskell}
-- | Underlying monad has to implement the MonadIO instance
instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = lift . liftIO
~~~~~~~~~~~~

  - IO is special since we can not "deconstruct" its values.
    - It has to be always on the bottom of the stack.

transformers - Identity
======================

  - Used to produce the final, non-transformer type.
  - Error, State, Reader, Writer.

~~~~~~~~~~~{.haskell}
type Writer w = WriterT w Identity
~~~~~~~~~~~

transformers
============

~~~~~~~~~~~~~~~
Monad
  IO
    Control.Monad.IO.Class
  Trans
    Control.Monad.Trans.Class
    Control.Monad.Trans.Cont
    Control.Monad.Trans.Error
    Control.Monad.Trans.Except
    Control.Monad.Trans.Identity
    Control.Monad.Trans.List
    Control.Monad.Trans.Maybe
    Control.Monad.Trans.RWS
      Control.Monad.Trans.RWS.Lazy
      Control.Monad.Trans.RWS.Strict
    Control.Monad.Trans.Reader
    Control.Monad.Trans.State
      Control.Monad.Trans.State.Lazy
      Control.Monad.Trans.State.Strict
    Control.Monad.Trans.Writer
      Control.Monad.Trans.Writer.Lazy
      Control.Monad.Trans.Writer.Strict

Data
  Functor
    Identity
~~~~~~~~~~~~~~~

transformers - monad stack
==========================

  - back to our example
  - adding more functionality to the computation.

~~~~~~~~~~~{.haskell}
-- | Counter monad stack.
type Counter a = ReaderT CounterConfig (StateT CounterState (WriterT CounterLog IO)) a

-- | Runner of the monad with provided configuration.
runCounter :: Counter a -> CounterConfig -> IO ((a, CounterState), CounterLog)
runCounter m config =
  let initialState = CounterState 0
  in
   runWriterT $ runStateT (runReaderT m config) initialState

-- | Lift writer operation.
counterTell :: CounterLog -> Counter ()
counterTell = lift . lift . tell

-- | Lift state get operation.
counterGet :: Counter CounterState
counterGet = lift get

-- | lift state put operation.
counterPut :: CounterState -> Counter ()
counterPut = lift . put
~~~~~~~~~~~

  - Reordering of the stack.

mtl (monad transformer library)
===============================

  - Extends transformers package.
  - Adding type classes which define the "effect basis" of the standard monads.
  - MonadState, MonadReader, MonadWriter, MonadError.
  - Different way of looking into the underlying monads.
  - For a CustomT we have to implement instances of all possible underlying monads.
  - Luckily, there are no that many useful basic monads. :)

mtl - package contents
=====================

~~~~~~~~~~~~~~~~~~
Control
  Monad
    Control.Monad.Cont
      Control.Monad.Cont.Class
    Control.Monad.Error
      Control.Monad.Error.Class
    Control.Monad.Identity
    Control.Monad.List
    Control.Monad.RWS
      Control.Monad.RWS.Class
      Control.Monad.RWS.Lazy
      Control.Monad.RWS.Strict
    Control.Monad.Reader
      Control.Monad.Reader.Class
    Control.Monad.State
      Control.Monad.State.Class
      Control.Monad.State.Lazy
      Control.Monad.State.Strict
    Control.Monad.Trans
    Control.Monad.Writer
      Control.Monad.Writer.Class
      Control.Monad.Writer.Lazy
      Control.Monad.Writer.Strict
~~~~~~~~~~~~~~~~~~

mtl - type classes
==================

  - used to bring the "effect basis" functions of a monad to the "surface"

~~~~~~~~~~~~~~~{.haskell}
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    -- | @'tell' w@ is an action that produces the output @w@.
    tell :: w -> m ()

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    tell = lift . tell
~~~~~~~~~~~~~~~

mtl - pre-implemented instances
===================

~~~~~~~~~~~~~~~{.haskell}
instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
    tell = id

instance MonadWriter w m => MonadWriter w (StateT s m) where
    tell = lift . tell

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    tell = lift . tell

instance (Error e, MonadWriter w m) => MonadWriter w (ErrorT e m) where
    tell = lift . tell

instance MonadWriter w m => MonadWriter w (ExceptT e m) where
    tell = lift . tell

instance MonadWriter w m => MonadWriter w (IdentityT m) where
    tell = lift . tell

instance MonadWriter w m => MonadWriter w (MaybeT m) where
    tell = lift . tell

~~~~~~~~~~~~~~~

mtl - monad stack example
=====================

  - same monad stack using mtl package

~~~~~~~~~~~~{.haskell}
-- | Our monad stack.
newtype Counter a = Counter {
  runCounter :: ReaderT CounterConfig (StateT CounterState (WriterT CounterLog IO)) a
  } deriving ( Monad, MonadIO, MonadReader CounterConfig
             , MonadState CounterState, MonadWriter CounterLog)

-- | Traverses the folder tree and counts number of entries.
--   Writes results into a writer log along the way.
countEntries :: FilePath -> Counter ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    -- gets the configuration
    cfg <- ask
    -- gets the current state
    st <- get
    -- records the log
    tell [(path, length contents)]
    forM_ contents $ \name -> do
      let newPath = path </> name
          depth = currentDepth st
      isDir <- liftIO . doesDirectoryExist $ newPath
      when (isDir && depth < maxDepth cfg) $ do
        -- updates the state
        put st {currentDepth = depth + 1}
        countEntries newPath
~~~~~~~~~~~~

mtl - positives / negatives
===============

  - Positive: for standard monads convenient instance implementations.
  - Positive: standard API, no need for custom names.
  - Negative: declaring new transformer FooT requires O(N^2) implementations of type class instances of MonadFoo.

X monad
=======

  - X monad is quite standard composition of monads.

~~~~~~~~~~~~~~{.haskell}
-- | The X monad, 'ReaderT' and 'StateT' transformers over 'IO'
-- encapsulating the window manager configuration and state,
-- respectively.
--
-- Dynamic components may be retrieved with 'get', static components
-- with 'ask'. With newtype deriving we get readers and state monads
-- instantiated on 'XConf' and 'XState' automatically.
--
newtype X a = X (ReaderT XConf (StateT XState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState XState, MonadReader XConf, Typeable)
~~~~~~~~~~~~~~

operational
===========

  - Operational semantics approach to monads based on the [Unimo framework](https://5900f98f-a-62cb3a1a-s-sites.googlegroups.com/site/chklin/research/unimo-icfp06.pdf?attachauth=ANoY7crFqU6lXTEUGdLMl_qxx_bymK_SXDajvp1E5Lzk_pGME24G8bdrSjN9ThKRc7RJXAffUFW4o2A-7-qzlEoXaARdGJVWceEdG-pROACDWi_2YGRM-eVOlfXseD8NVbIACWkP_NtPUwWS5c9n2xo9J2WNPWG8ZKpxMUiRIcRyyk8aQfAA1R8VV8o9sk18cA2LAFMOBpJ0sSi4SEchyOnJMG8JBPPqOeUAe9flskO05Nj3k9yrY08%3D&attredirects=0).
  - In comparison to the denotational definition, enforces monadic laws.
  - Generic for all monadic effects through the "observer" function.
  - View monads as a sequence of instructions to be executed by a machine.
  - Avoids building every monad from scratch.

operational/unimo - monad definition
=============

  - denotational list definition

~~~~~~~~~~~~~{.haskell}
instance Monad [] where
  return x     = [x]
  [] >>= _     = []
  (x:xs) >>= k = k x ++ (xs >>= k)

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus u v = u ++ v
~~~~~~~~~~~~~

  - operational definition

~~~~~~~~~~~~~{.haskell}
-- | Definition of the monadic data structure
data Plus a
  -- basic structure
  = Unit a
  | forall b. Bind (Plus b) (b -> Plus a)
  -- effects
  | Zero
  | Plus (Plus a) (Plus a)

instance Monad Plus where
  return = Unit
  (>>=) = Bind

instance MonadPlus Plus where
  mzero = Zero
  mplus = Plus

-- evaluator of the monadic data structure
run_list :: Plus a -> [a]
run_list (Unit a)              = [a]
run_list m@Zero                = run_list (Bind m Unit)
run list m@(Plus _ _)          = run_list (Bind m Unit)
run_list (Bind (Unit v) k)     = run_list (k v)
run_list (Bind (Bind m k) g)   = run_list (Bind m cont)
  where cont v = Bind (k v) g
run_list (Bind Zero _)         = [ ]
run_list (Bind (Plus m n) k)   = ms ++ ns
  where ms = run_list (Bind m k)
        ns = run_list (Bind n k)
~~~~~~~~~~~~~

operational/unimo - general monad definition
============================

~~~~~~~~~~~~~~~~~{.haskell}
data Unimo r a
  = Unit a
  | Effect (r (Unimo r) a)
  | forall b. Bind (Unimo r b) (b -> Unimo r a)

instance Monad (Unimo r) where
  return = Unit
  (>>=) = Bind

type BindOp r a v = forall b.
  r (Unimo r) b -> (b -> Unimo r a) -> v

type Observer r a v =
  (a -> v) -> BindOp r a v -> Unimo r a -> v

-- | Monad observer function
observe_monad :: Observer r a v
observe_monad unit_op bind_op = eval where
  eval (Unit v)               = unit_op v
  eval (Effect e)             = e `bind_op` Unit
  eval (Bind (Effect e) k)    = e `bind_op` k
  eval (Bind (Unit v) k)      = eval (k v)
  eval (Bind (Bind m k) g)    = eval (Bind m cont)
    where cont v = Bind (k v) g
~~~~~~~~~~~~~~~~~

  - operational - compact List definition

~~~~~~~~~~~~~~~~~{.haskell}
-- | Expressing just effect part of the monad
data PlusE m a
  = Zero
  | Plus (m a) (m a)

-- | Full monadic value.
type Plus = Unimo PlusE

run_list :: Plus a -> [a]
run_list = observe monad unit_op bind_op where
  unit_op v = [v]
  bind_op Zero = [ ]
  bind_op (Plus m n) k =
    let ms = run_list (Bind m k)
        ns = run_list (Bind n k)
    in ms ++ ns
~~~~~~~~~~~~~~~~~

operational - going further
============

  - [Unimo framework](https://5900f98f-a-62cb3a1a-s-sites.googlegroups.com/site/chklin/research/unimo-icfp06.pdf?attachauth=ANoY7crFqU6lXTEUGdLMl_qxx_bymK_SXDajvp1E5Lzk_pGME24G8bdrSjN9ThKRc7RJXAffUFW4o2A-7-qzlEoXaARdGJVWceEdG-pROACDWi_2YGRM-eVOlfXseD8NVbIACWkP_NtPUwWS5c9n2xo9J2WNPWG8ZKpxMUiRIcRyyk8aQfAA1R8VV8o9sk18cA2LAFMOBpJ0sSi4SEchyOnJMG8JBPPqOeUAe9flskO05Nj3k9yrY08%3D&attredirects=0)
  - [The Operational Monad Tutorial](http://apfelmus.nfshost.com/articles/operational-monad.html )
  - [Operational package](https://github.com/HeinrichApfelmus/operational )
  - [Examples](https://github.com/HeinrichApfelmus/operational/tree/master/doc/examples)

Resources
===========

  - [All about monads](http://www.haskell.org/haskellwiki/All_About_Monads)
  - [Real World Haskell](http://book.realworldhaskell.org/read/monad-transformers.html)
  - [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia#Laws_3)
  - [Haskell mailing list archive](http://www.haskell.org/pipermail/libraries/2010-September/014281.html)
  - [Applicatives compose, monads do not](http://carlo-hamalainen.net/blog/2014/1/2/applicatives-compose-monads-do-not)
