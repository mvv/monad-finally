{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A generalization of "Control.Exception" from the 'IO' monad to
-- 'MonadRecover' 'SomeException'.
module Control.Monad.Exception
  (
  -- * Throwing exceptions
    MonadThrow
  , exception
  , throw
  , throwIO
  , ioError
  , throwTo
  -- * Catching exceptions
  , MonadCatch
  , catch
  , handle
  , catchJust
  , handleJust
  , Handler(..)
  , catches
  , handles
  , try
  , tryJust
  , evaluateIO
  -- * Asynchronous exception control
  , MonadMask(..)
  , liftGetMaskingState
  , liftWithMaskingState
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , interruptible
  , allowInterrupt
  -- * Re-exports
  , module Control.Monad.Abort.Class
  , module Control.Monad.Finally
  , module Control.Exception
  ) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#else
import Prelude hiding (ioError)
#endif
import Data.Monoid (Monoid)
import Control.Applicative (Applicative, (<$>))
import Control.Monad (join, liftM)
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control (MonadTransControl(..))
import Control.Monad.Trans.Abort hiding (abort, recover)
import Control.Monad.Trans.Finish
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Abort.Class
import Control.Monad.Finally
import Control.Concurrent (ThreadId)
import Control.Exception hiding (
  evaluate, throw, throwIO, ioError, throwTo, catch, catchJust, handle,
  handleJust, Handler(..), catches, try, tryJust, finally, onException,
#if !MIN_VERSION_base(4,7,0)
  block, unblock, blocked,
#endif
  getMaskingState, mask, mask_, uninterruptibleMask, uninterruptibleMask_,
#if MIN_VERSION_base(4,9,0)
  interruptible,
#endif
  allowInterrupt, bracket, bracket_, bracketOnError)
import qualified Control.Exception as E
import GHC.Base (maskAsyncExceptions#, maskUninterruptible#,
                 unmaskAsyncExceptions#)
import GHC.IO (IO(..))

-- | An alias for 'MonadAbort' 'SomeException' /μ/.
type MonadThrow μ = MonadAbort SomeException μ

-- | Throw an exception from pure code.
exception ∷ Exception e ⇒ e → α
exception = E.throw
{-# INLINE exception #-}

-- | Throw an exception from monadic code. An alias for
-- @'abort' . 'toException'@.
throw ∷ (MonadThrow μ, Exception e) ⇒ e → μ α
throw = abort . toException
{-# INLINE throw #-}

-- | Thow an exception from the 'IO' monad.
throwIO ∷ (MonadBase IO μ, Exception e) ⇒ e → μ α
throwIO = liftBase . E.throwIO
{-# INLINE throwIO #-}

-- | Raise an 'IOError' in the 'IO' monad.
ioError ∷ MonadBase IO μ ⇒ IOError → μ α
ioError = liftBase . E.ioError
{-# INLINE ioError #-}

-- | Raise an exception in the target thread. See 'E.throwTo'.
throwTo ∷ (MonadBase IO μ, Exception e) ⇒ ThreadId → e → μ ()
throwTo = fmap liftBase . E.throwTo
{-# INLINE throwTo #-}

-- | An alias for 'MonadRecover' 'SomeException' /μ/.
type MonadCatch μ = MonadRecover SomeException μ

-- | Recover from the specified type of exceptions.
catch ∷ (MonadCatch μ, Exception e) ⇒ μ α → (e → μ α) → μ α
catch m h = recover m $ \e → maybe (throw e) h (fromException e)
{-# INLINE catch #-}

-- | An alias for @'flip' 'catch'@.
handle ∷ (MonadCatch μ, Exception e) ⇒ (e → μ α) → μ α → μ α
handle = flip catch
{-# INLINE handle #-}

-- | Recover from exceptions that satisfy the provided predicate.
catchJust ∷ (MonadCatch μ, Exception e)
          ⇒ (e → Maybe β) -- ^ Exception predicate
          → μ α           -- ^ Main computation
          → (β → μ α)     -- ^ Exception handler
          → μ α
catchJust f m h = catch m $ \e → maybe (throw e) h $ f e
{-# INLINE catchJust #-}

-- | An alias for @'flip' . 'catchJust'@.
handleJust ∷ (MonadCatch μ, Exception e)
           ⇒ (e → Maybe β) → (β → μ α) → μ α → μ α
handleJust = flip . catchJust
{-# INLINE handleJust #-}

-- | Exception handler.
data Handler μ α = ∀ e . Exception e ⇒ Handler (e → μ α)

instance Functor μ ⇒ Functor (Handler μ) where
  fmap f (Handler h) = Handler (fmap f . h)
  {-# INLINE fmap #-}

-- | Recover from exceptions by sequentually trying to apply the provided
-- handlers.
catches ∷ MonadCatch μ ⇒ μ α → [Handler μ α] → μ α
catches m = recover m . hl
  where hl [] e = abort e
        hl (Handler h : hs) e = maybe (hl hs e) h $ fromException e

-- | An alias for @'flip' 'catches'@.
handles ∷ MonadCatch μ ⇒ [Handler μ α] → μ α → μ α
handles = flip catches
{-# INLINE handles #-}

-- | Recover from exceptions of the spesified type, wrapping them into
-- 'Left'.
try ∷ (MonadCatch μ, Exception e) ⇒ μ α → μ (Either e α)
try m = catch (Right <$> m) (return . Left)
{-# INLINE try #-}

-- | Recover from exceptions that satisfy the provided predicate, wrapping
-- them into 'Left'.
tryJust ∷ (MonadCatch μ, Exception e)
        ⇒ (e → Maybe β) -- ^ Exception predicate
        → μ α           -- ^ Main compuration
        → μ (Either β α)
tryJust h m = catch (Right <$> m) $ \e →
  maybe (throw e) (return . Left) (h e)
{-# INLINE tryJust #-}

-- | Evalute the argument to weak head normal form.
evaluateIO ∷ MonadBase IO μ ⇒ α → μ α
evaluateIO = liftBase . E.evaluate
{-# INLINE evaluateIO #-}

-- | A class of monads that support masking of asynchronous exceptions.
class (Applicative μ, Monad μ) ⇒ MonadMask μ where
  -- | Get the current masking state.
  getMaskingState ∷ μ MaskingState
  default getMaskingState ∷ (MonadMask η, MonadTrans t, μ ~ t η)
                          ⇒ μ MaskingState
  getMaskingState = liftGetMaskingState
  {-# INLINE getMaskingState #-}
  -- | Run the provided computation with the specified 'MaskingState'.
  withMaskingState ∷ MaskingState → μ α → μ α
  default withMaskingState ∷ (MonadMask η, MonadTransControl t, μ ~ t η)
                           ⇒ MaskingState → μ α → μ α
  withMaskingState = liftWithMaskingState
  {-# INLINE withMaskingState #-}

-- | Lift 'getMaskingState' through a monad transformer.
liftGetMaskingState ∷ (MonadMask μ, MonadTrans t) ⇒ t μ MaskingState
liftGetMaskingState = lift getMaskingState
{-# INLINE liftGetMaskingState #-}

-- | Lift 'withMaskingState' through a monad transformer.
liftWithMaskingState ∷ (MonadTransControl t, MonadMask μ, Monad (t μ))
                     ⇒ MaskingState → t μ α → t μ α
liftWithMaskingState ms m =
  join $ liftM (restoreT . return) $ liftWith $ \run →
    withMaskingState ms (run m)
{-# INLINE liftWithMaskingState #-}

instance MonadMask IO where
  getMaskingState = E.getMaskingState
  withMaskingState Unmasked (IO io) = IO $ unmaskAsyncExceptions# io
  withMaskingState MaskedInterruptible (IO io) = IO $ maskAsyncExceptions# io
  withMaskingState MaskedUninterruptible (IO io) = IO $ maskUninterruptible# io

instance MonadMask μ ⇒ MonadMask (MaybeT μ)
instance MonadMask μ ⇒ MonadMask (ListT μ)
instance MonadMask μ ⇒ MonadMask (AbortT e μ)
instance MonadMask μ ⇒ MonadMask (FinishT β μ)
instance (MonadMask μ, Error e) ⇒ MonadMask (ErrorT e μ)
instance MonadMask μ ⇒ MonadMask (ExceptT e μ)
instance MonadMask μ ⇒ MonadMask (ReaderT r μ)
instance MonadMask μ ⇒ MonadMask (L.StateT s μ)
instance MonadMask μ ⇒ MonadMask (S.StateT s μ)
instance (MonadMask μ, Monoid w) ⇒ MonadMask (L.WriterT w μ)
instance (MonadMask μ, Monoid w) ⇒ MonadMask (S.WriterT w μ)
instance (MonadMask μ, Monoid w) ⇒ MonadMask (L.RWST r w s μ)
instance (MonadMask μ, Monoid w) ⇒ MonadMask (S.RWST r w s μ)

-- | Prevents asynchronous exceptions from being raised within the provided
-- computation. Blocking operations can still be interrupted. Supplies the
-- computation with @'withMaskingState' s@, where @s@ is the current masking
-- state.
mask ∷ ∀ μ α . MonadMask μ
     ⇒ ((∀ η β . MonadMask η ⇒ η β → η β) → μ α) → μ α
mask f = getMaskingState >>= \case
  Unmasked →
    withMaskingState MaskedInterruptible $ f (withMaskingState Unmasked)
  MaskedInterruptible →
    f (withMaskingState MaskedInterruptible)
  MaskedUninterruptible →
    f (withMaskingState MaskedUninterruptible)
 
-- | An alias for @'mask' . 'const'@.
mask_ ∷ ∀ μ α . MonadMask μ ⇒ μ α → μ α
mask_ = mask . const
{-# INLINE mask_ #-}

-- | Prevents asynchronous exceptions from being raised within the provided
-- computation. Also prevents blocking operations from being interrupted.
-- Supplies the computation with @'withMaskingState' s@, where @s@ is the
-- current masking state.
uninterruptibleMask ∷ MonadMask μ
                    ⇒ ((∀ η β . MonadMask η ⇒ η β → η β) → μ α) → μ α
uninterruptibleMask f = getMaskingState >>= \case
  Unmasked →
    withMaskingState MaskedUninterruptible $ f (withMaskingState Unmasked)
  MaskedInterruptible →
    withMaskingState MaskedUninterruptible $
      f (withMaskingState MaskedInterruptible)
  MaskedUninterruptible →
    f (withMaskingState MaskedUninterruptible)

-- | An alias for @'uninterruptibleMask' . 'const'@.
uninterruptibleMask_ ∷ MonadMask μ ⇒ μ α → μ α
uninterruptibleMask_ = uninterruptibleMask . const
{-# INLINE uninterruptibleMask_ #-}

-- | Allow asynchronous exceptions to be raised within the provided
-- computation, even if the current masking state is 'MaskedInterruptible'
-- (but not in 'MaskedUninterruptible' state).
interruptible ∷ MonadMask μ ⇒ μ α → μ α
interruptible m = getMaskingState >>= \case
  Unmasked → m
  MaskedInterruptible → withMaskingState Unmasked m
  MaskedUninterruptible → m

-- | An alias for @'interruptible' ('return' ())@.
allowInterrupt ∷ MonadMask μ ⇒ μ ()
allowInterrupt = interruptible $ return ()
{-# INLINE allowInterrupt #-}
