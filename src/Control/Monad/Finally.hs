{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Guarding monadic computations with cleanup actions.
module Control.Monad.Finally
  ( MonadFinally(..)
  , chainCleanups
  , finallyMany
  , onEscape
  , onEscapeMany
  , bracket_
  , bracketOnEscape
  ) where

#if !MIN_VERSION_base(4,8,0)
import Prelude hiding (mapM, forM)
#endif
import Data.Functor.Identity
import Data.Monoid (Monoid(..))
import Data.Traversable (mapM, forM)
import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Monad (join)
import Control.Monad.Trans.Maybe
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.Error
#endif
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as LW
import qualified Control.Monad.Trans.Writer.Strict as SW
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Abort
import Control.Monad.Trans.Finish
import qualified Control.Exception as E

-- | Class of monads that support guarding computations with cleanup actions.
class (Applicative μ, Monad μ) ⇒ MonadFinally μ where
#if __GLASGOW_HASKELL__ >= 707
  {-# MINIMAL finally' | bracket' #-}
#endif
  -- | @'finally'' m f@ runs computation @m@ and then
  --
  -- 1. runs @f ('Just' x)@ if @m@ produced a result @x@. The result of that is
  -- returned alongside with @x@.
  -- 2. runs @f 'Nothing'@ otherwise.
  finally' ∷ μ α → (Maybe α → μ β) → μ (α, β)
  finally' m f = bracket' (return ()) (const f) (const m)
  -- | A simplified version of 'finally'' in which the cleanup action
  -- does not care about the result of the main computation. The default
  -- implementation is
  --
  -- @
  --     'finally' m = 'fmap' 'fst' . 'finally'' m . 'const'
  -- @
  finally  ∷ μ α → μ β → μ α
  finally m = fmap fst . finally' m . const
  -- | Safely acquire a resource and use it in a computation, releasing it
  -- even when the computation does not produce a result.
  bracket' ∷ μ r                 -- ^ Acquire resource
           → (r → Maybe α → μ β) -- ^ Release resource
           → (r → μ α)           -- ^ Main computation
           → μ (α, β)
  bracket' acquire release m = do
    r ← acquire
    finally' (m r) (release r)
  -- | A simplified version of 'bracket'' in which the releasing action
  -- does not care about the result of the main computation. The default
  -- implementation is
  --
  -- @
  --     'bracket' acquire release =
  --       'fmap' 'fst' . 'bracket'' acquire ('const' . release)
  -- @
  bracket ∷ μ r → (r → μ β) → (r → μ α) → μ α
  bracket acquire release = fmap fst . bracket' acquire (const . release)

instance MonadFinally Identity where
  finally' m f = do
    mr ← m
    return (mr, runIdentity $ f $ Just mr)

instance MonadFinally IO where
  finally' m f = E.mask $ \restore → do
    mr ← restore m `E.onException` f Nothing
    fr ← f $ Just mr
    return (mr, fr)
  bracket' acquire release m = E.mask $ \restore → do
    ar ← acquire
    mr ← restore (m ar) `E.onException` release ar Nothing
    rr ← release ar $ Just mr
    return (mr, rr)

instance MonadFinally μ ⇒ MonadFinally (MaybeT μ) where
  finally' m f = MaybeT $ do
    (mr, fr) ← finally' (runMaybeT m) (runMaybeT . f . join)
    return $ (,) <$> mr <*> fr
  bracket' acquire release m = MaybeT $ do
    (mr, rr) ← bracket' (runMaybeT acquire)
                        (\case
                           Just ar → runMaybeT . release ar . join
                           Nothing → const (return Nothing))
                        (fmap join . mapM (runMaybeT . m))
    return $ (,) <$> mr <*> rr

justRight ∷ Maybe (Either e α) → Maybe α
justRight (Just (Right a)) = Just a
justRight _                = Nothing

instance MonadFinally μ ⇒ MonadFinally (AbortT e μ) where
  finally' m f = AbortT $ do
    (mr, fr) ← finally' (runAbortT m) (runAbortT . f . justRight)
    return $ (,) <$> mr <*> fr
  bracket' acquire release m = AbortT $ do
    (mr, rr) ← bracket' (runAbortT acquire)
                        (\case
                          Right ar → runAbortT . release ar . justRight
                          Left e → const (return (Left e)))
                        (fmap join . mapM (runAbortT . m))
    return $ (,) <$> mr <*> rr

instance MonadFinally μ ⇒ MonadFinally (FinishT β μ) where
  finally' m f = FinishT $ do
    (mr, fr) ← finally' (runFinishT m) (runFinishT . f . justRight)
    return $ (,) <$> mr <*> fr
  bracket' acquire release m = FinishT $ do
    (mr, rr) ← bracket' (runFinishT acquire)
                        (\case
                          Right ar → runFinishT . release ar . justRight
                          Left e → const (return (Left e)))
                        (fmap join . mapM (runFinishT . m))
    return $ (,) <$> mr <*> rr

#if !MIN_VERSION_transformers(0,6,0)
instance (MonadFinally μ, Error e) ⇒ MonadFinally (ErrorT e μ) where
  finally' m f = ErrorT $ do
    (mr, fr) ← finally' (runErrorT m) (runErrorT . f . justRight)
    return $ (,) <$> mr <*> fr
  bracket' acquire release m = ErrorT $ do
    (mr, rr) ← bracket' (runErrorT acquire)
                        (\case
                          Right ar → runErrorT . release ar . justRight
                          Left e → const (return (Left e)))
                        (fmap join . mapM (runErrorT . m))
    return $ (,) <$> mr <*> rr
#endif

instance MonadFinally μ ⇒ MonadFinally (ExceptT e μ) where
  finally' m f = ExceptT $ do
    (mr, fr) ← finally' (runExceptT m) (runExceptT . f . justRight)
    return $ (,) <$> mr <*> fr
  bracket' acquire release m = ExceptT $ do
    (mr, rr) ← bracket' (runExceptT acquire)
                        (\case
                          Right ar → runExceptT . release ar . justRight
                          Left e → const (return (Left e)))
                        (fmap join . mapM (runExceptT . m))
    return $ (,) <$> mr <*> rr

instance MonadFinally μ ⇒ MonadFinally (ReaderT r μ) where
  finally' m f = ReaderT $ \r →
    finally' (runReaderT m r) ((`runReaderT` r) . f)
  bracket' acquire release m = ReaderT $ \r →
    bracket' (runReaderT acquire r)
             (fmap (`runReaderT` r) . release)
             ((`runReaderT` r) . m)

instance MonadFinally μ ⇒ MonadFinally (L.StateT s μ) where
  finally' m f = L.StateT $ \s → do
    ~(~(mr, _), ~(fr, s'')) ← finally' (L.runStateT m s) $ \case
      Just ~(a, s') → L.runStateT (f $ Just a) s'
      Nothing       → L.runStateT (f Nothing) s
    return ((mr, fr), s'')
  bracket' acquire release m = L.StateT $ \s → do
    ~(~(mr, _), ~(fr, s'')) ←
      bracket' (L.runStateT acquire s)
               (\ ~(ar, s') → \case
                  Just ~(mr, s'') → L.runStateT (release ar (Just mr)) s''
                  Nothing         → L.runStateT (release ar Nothing) s')
               (\ ~(ar, s') → L.runStateT (m ar) s')
    return ((mr, fr), s'')

instance MonadFinally μ ⇒ MonadFinally (S.StateT s μ) where
  finally' m f = S.StateT $ \s → do
    ((mr, _), (fr, s''')) ← finally' (S.runStateT m s) $ \case
      Just (a, s') → S.runStateT (f $ Just a) s'
      Nothing      → S.runStateT (f Nothing) s
    return ((mr, fr), s''')
  bracket' acquire release m = S.StateT $ \s → do
    ((mr, _), (fr, s''')) ←
      bracket' (S.runStateT acquire s)
               (\(ar, s') → \case
                  Just ~(mr, s'') → S.runStateT (release ar (Just mr)) s''
                  Nothing         → S.runStateT (release ar Nothing) s')
               (\(ar, s') → S.runStateT (m ar) s')
    return ((mr, fr), s''')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (LW.WriterT w μ) where
  finally' m f = LW.WriterT $ do
    ~(~(mr, w), ~(fr, w')) ← finally' (LW.runWriterT m) $
      LW.runWriterT . f . fmap fst
    return ((mr, fr), w `mappend` w')
  bracket' acquire release m = LW.WriterT $ do
    ~(~(mr, w), ~(fr, w')) ←
      bracket' (LW.runWriterT acquire)
               (\ ~(ar, _) → \case
                  Just ~(mr, _) → LW.runWriterT (release ar (Just mr))
                  Nothing → LW.runWriterT (release ar Nothing))
               (\ ~(ar, aw) → LW.runWriterT (LW.tell aw >> m ar))
    return ((mr, fr), w `mappend` w')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (SW.WriterT w μ) where
  finally' m f = SW.WriterT $ do
    ((mr, w), (fr, w')) ← finally' (SW.runWriterT m) $ \mbr → case mbr of
      Just (a, _) → SW.runWriterT $ f $ Just a
      Nothing     → SW.runWriterT $ f Nothing
    return ((mr, fr), w `mappend` w')
  bracket' acquire release m = SW.WriterT $ do
    ((mr, w), (fr, w')) ←
      bracket' (SW.runWriterT acquire)
               (\(ar, _) → \case
                  Just (mr, _) → SW.runWriterT (release ar (Just mr))
                  Nothing → SW.runWriterT (release ar Nothing))
               (\(ar, aw) → SW.runWriterT (SW.tell aw >> m ar))
    return ((mr, fr), w `mappend` w')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (L.RWST r w s μ) where
  finally' m f = L.RWST $ \r s → do
    ~(~(mr, _, w), ~(fr, s'', w')) ← finally' (L.runRWST m r s) $ \case
      Just ~(mr, s', _) → L.runRWST (f $ Just mr) r s'
      Nothing → L.runRWST (f Nothing) r s
    return ((mr, fr), s'', w `mappend` w')
  bracket' acquire release m = L.RWST $ \r s → do
    ~(~(mr, _, w), ~(fr, s''', w')) ←
      bracket' (L.runRWST acquire r s)
               (\ ~(ar, s', _) → \case 
                  Just ~(mr, s'', _) → L.runRWST (release ar $ Just mr) r s''
                  Nothing → L.runRWST (release ar Nothing) r s')
               (\ ~(ar, s', aw) → L.runRWST (L.tell aw >> m ar) r s')
    return ((mr, fr), s''', w `mappend` w')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (S.RWST r w s μ) where
  finally' m f = S.RWST $ \r s → do
    ((mr, _, w), (fr, s'', w')) ← finally' (S.runRWST m r s) $ \case
      Just (a, s', _) → S.runRWST (f $ Just a) r s'
      Nothing         → S.runRWST (f Nothing) r s
    return ((mr, fr), s'', w `mappend` w')
  bracket' acquire release m = S.RWST $ \r s → do
    ((mr, _, w), (fr, s''', w')) ←
      bracket' (S.runRWST acquire r s)
               (\(ar, s', _) → \case 
                  Just (mr, s'', _) → S.runRWST (release ar $ Just mr) r s''
                  Nothing → S.runRWST (release ar Nothing) r s')
               (\(ar, s', aw) → S.runRWST (S.tell aw >> m ar) r s')
    return ((mr, fr), s''', w `mappend` w')

instance (MonadFinally μ, Monoid w) ⇒ MonadFinally (AccumT w μ) where
  finally' m f = AccumT $ \w → do
    ~(~(mr, _), ~(fr, w'')) ← finally' (runAccumT m w) $ \case
      Just ~(a, w') → runAccumT (f $ Just a) w'
      Nothing       → runAccumT (f Nothing) w
    return ((mr, fr), w'')
  bracket' acquire release m = AccumT $ \w → do
    ~(~(mr, _), ~(fr, w''')) ←
      bracket' (runAccumT acquire w)
               (\ ~(ar, w') → \case
                  Just ~(mr, w'') → runAccumT (release ar (Just mr)) w''
                  Nothing → runAccumT (release ar Nothing) w')
               (\ ~(ar, w') → runAccumT (m ar) w')
    return ((mr, fr), w''')

-- | Run the provided list of cleanup actions sequentually, attempting to run
-- the next action even if the previous one did not produce a result.
chainCleanups ∷ MonadFinally μ ⇒ [μ α] → μ ()
chainCleanups []       = return ()
chainCleanups (m : ms) = finally (() <$ m) $ chainCleanups ms

-- | A variant of 'finally' that combines multiple cleanup actions with
-- 'chainCleanups'.
finallyMany ∷ MonadFinally μ ⇒ μ α → [μ β] → μ α
finallyMany m = finally m . chainCleanups
{-# INLINE finallyMany #-}

-- | @'onEscape' m c@ runs computation @m@ and then, if it did not produce
-- a result, runs computation @c@.
onEscape ∷ MonadFinally μ ⇒ μ α → μ β → μ α
onEscape m f = fmap fst $ finally' m $ maybe (() <$ f) (const $ return ())
{-# INLINE onEscape #-}

-- | A variant of `onEscape` that combines multiple cleanup actions with
-- 'chainCleanups'.
onEscapeMany ∷ MonadFinally μ ⇒ μ α → [μ β] → μ α
onEscapeMany m = onEscape m . chainCleanups

-- | A variant of 'bracket' where acquired value is not needed (e.g. using
-- a static resource).
bracket_ ∷ MonadFinally μ
         ⇒ μ r -- ^ Acquire resource
         → μ β -- ^ Release resource
         → μ α -- ^ Main computation
         → μ α
bracket_ acquire release = bracket acquire (const release) . const
{-# INLINE bracket_ #-}

-- | A variant of 'bracket' that releases the acquired resource only when
-- the main computation does not produce a value.
bracketOnEscape ∷ MonadFinally μ
                ⇒ μ r       -- ^ Acquire resource
                → (r → μ β) -- ^ Release resource
                → (r → μ α) -- ^ Main computation
                → μ α
bracketOnEscape acquire release = fmap fst . bracket' acquire release'
  where release' _  (Just _) = return Nothing
        release' ar Nothing  = Just <$> release ar
