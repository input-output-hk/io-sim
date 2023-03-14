{-# LANGUAGE RankNTypes      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Class.MonadThrow.Trans () where

import           Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
 
import           Control.Monad.Class.MonadThrow

--
-- ExceptT Instances
--
-- These all follow the @exceptions@ package to the letter
--

instance MonadCatch m => MonadThrow (ExceptT e m) where
  throwIO = lift . throwIO

instance MonadCatch m => MonadCatch (ExceptT e m) where
  catch (ExceptT m) f = ExceptT $ catch m (runExceptT . f)

  generalBracket acquire release use = ExceptT $ do
    (eb, ec) <- generalBracket
      (runExceptT acquire)
      (\eresource exitCase -> case eresource of
        Left e -> return (Left e) -- nothing to release, acquire didn't succeed
        Right resource -> case exitCase of
          ExitCaseSuccess (Right b) -> runExceptT (release resource (ExitCaseSuccess b))
          ExitCaseException e       -> runExceptT (release resource (ExitCaseException e))
          _                         -> runExceptT (release resource ExitCaseAbort))
      (either (return . Left) (runExceptT . use))
    return $ do
      -- The order in which we perform those two 'Either' effects determines
      -- which error will win if they are both 'Left's. We want the error from
      -- 'release' to win.
      c <- ec
      b <- eb
      return (b, c)

instance MonadMask m => MonadMask (ExceptT e m) where
  mask f = ExceptT $ mask $ \u -> runExceptT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ExceptT e m a -> ExceptT e m a
      q u (ExceptT b) = ExceptT (u b)
  uninterruptibleMask f = ExceptT $ uninterruptibleMask $ \u -> runExceptT $ f (q u)
    where
      q :: (m (Either e a) -> m (Either e a))
        -> ExceptT e m a -> ExceptT e m a
      q u (ExceptT b) = ExceptT (u b)

--
-- Lazy.WriterT instances
--

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadThrow (Lazy.WriterT w m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadCatch (Lazy.WriterT w m) where
  catch (Lazy.WriterT m) f = Lazy.WriterT $ catch m (Lazy.runWriterT . f)

  generalBracket acquire release use = Lazy.WriterT $ fmap f $
      generalBracket
        (Lazy.runWriterT acquire)
        (\(resource, w) e ->
          case e of
            ExitCaseSuccess (b, w') ->
              g w' <$> Lazy.runWriterT (release resource (ExitCaseSuccess b)) 
            ExitCaseException err ->
              g w  <$> Lazy.runWriterT (release resource (ExitCaseException err))
            ExitCaseAbort ->
              g w  <$> Lazy.runWriterT (release resource ExitCaseAbort))
        (\(resource, w)   -> g w <$> Lazy.runWriterT (use resource))
    where f ((x,_),(y,w)) = ((x,y),w)
          g w (a,w') = (a,w<>w')

-- | @since 1.0.0.0
instance (Monoid w, MonadMask m) => MonadMask (Lazy.WriterT w m) where
  mask f = Lazy.WriterT $ mask $ \u -> Lazy.runWriterT $ f (q u)
    where
      q :: (forall x. m x -> m x)
        -> Lazy.WriterT w m a -> Lazy.WriterT w m a
      q u (Lazy.WriterT b) = Lazy.WriterT (u b)
  uninterruptibleMask f = Lazy.WriterT $ uninterruptibleMask $ \u -> Lazy.runWriterT $ f (q u)
    where
      q :: (forall x. m x -> m x)
        -> Lazy.WriterT w m a -> Lazy.WriterT w m a
      q u (Lazy.WriterT b) = Lazy.WriterT (u b)

--
-- Strict.WriterT instances
--

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadThrow (Strict.WriterT w m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadCatch (Strict.WriterT w m) where
  catch (Strict.WriterT m) f = Strict.WriterT $ catch m (Strict.runWriterT . f)

  generalBracket acquire release use = Strict.WriterT $ fmap f $
      generalBracket
        (Strict.runWriterT acquire)
        (\(resource, w) e ->
          case e of
            ExitCaseSuccess (b, w') ->
              g w' <$> Strict.runWriterT (release resource (ExitCaseSuccess b)) 
            ExitCaseException err ->
              g w  <$> Strict.runWriterT (release resource (ExitCaseException err))
            ExitCaseAbort ->
              g w  <$> Strict.runWriterT (release resource ExitCaseAbort))
        (\(resource, w)   -> g w <$> Strict.runWriterT (use resource))
    where f ((x,_),(y,w)) = ((x,y),w)
          g w (a,w') = (a,w<>w')

-- | @since 1.0.0.0
instance (Monoid w, MonadMask m) => MonadMask (Strict.WriterT w m) where
  mask f = Strict.WriterT $ mask $ \u -> Strict.runWriterT $ f (q u)
    where
      q :: (forall x. m x -> m x)
        -> Strict.WriterT w m a -> Strict.WriterT w m a
      q u (Strict.WriterT b) = Strict.WriterT (u b)
  uninterruptibleMask f = Strict.WriterT $ uninterruptibleMask $ \u -> Strict.runWriterT $ f (q u)
    where
      q :: (forall x. m x -> m x)
        -> Strict.WriterT w m a -> Strict.WriterT w m a
      q u (Strict.WriterT b) = Strict.WriterT (u b)


--
-- Lazy.RWST Instances
--

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadThrow (Lazy.RWST r w s m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadCatch (Lazy.RWST r w s m) where
  catch (Lazy.RWST m) f = Lazy.RWST $ \r s -> catch (m r s) (\e -> Lazy.runRWST (f e) r s)

  -- | general bracket ignores the state produced by the release callback
  generalBracket acquire release use = Lazy.RWST $ \r s ->
      f <$> generalBracket
              (Lazy.runRWST acquire r s)
              (\(resource, s', w') e ->
                case e of
                  ExitCaseSuccess (b, s'', w'') ->
                    g w'' <$> Lazy.runRWST (release resource (ExitCaseSuccess b)) r s''
                  ExitCaseException err ->
                    g w'  <$> Lazy.runRWST (release resource (ExitCaseException err)) r s'
                  ExitCaseAbort ->
                    g w'  <$> Lazy.runRWST (release resource  ExitCaseAbort) r s')
              (\(a, s', w')   -> g w' <$> Lazy.runRWST (use a) r s')
    where
      f ((x,_,_),(y,s,w)) = ((x,y),s,w)
      g w (x,s,w') = (x,s,w<>w')

-- | @since 1.0.0.0
instance (Monoid w, MonadMask m) => MonadMask (Lazy.RWST r w s m) where
  mask f = Lazy.RWST $ \r s -> mask $ \u -> Lazy.runRWST (f (q u)) r s
    where
      q :: (forall x. m x -> m x)
        -> Lazy.RWST r w s m a -> Lazy.RWST r w s m a
      q u (Lazy.RWST b) = Lazy.RWST $ \r s -> u (b r s)
  uninterruptibleMask f = Lazy.RWST $ \r s -> uninterruptibleMask $ \u -> Lazy.runRWST (f (q u)) r s
    where
      q :: (forall x. m x -> m x)
        -> Lazy.RWST r w s m a -> Lazy.RWST r w s m a
      q u (Lazy.RWST b) = Lazy.RWST $ \r s -> u (b r s)


--
-- Strict.RWST Instances
--

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadThrow (Strict.RWST r w s m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadCatch (Strict.RWST r w s m) where
  catch (Strict.RWST m) f = Strict.RWST $ \r s -> catch (m r s) (\e -> Strict.runRWST (f e) r s)

  -- | general bracket ignores the state produced by the release callback
  generalBracket acquire release use = Strict.RWST $ \r s ->
      f <$> generalBracket
              (Strict.runRWST acquire r s)
              (\(resource, s', w') e ->
                case e of
                  ExitCaseSuccess (b, s'', w'') ->
                    g w'' <$> Strict.runRWST (release resource (ExitCaseSuccess b)) r s''
                  ExitCaseException err ->
                    g w'  <$> Strict.runRWST (release resource (ExitCaseException err)) r s'
                  ExitCaseAbort ->
                    g w'  <$> Strict.runRWST (release resource  ExitCaseAbort) r s')
              (\(a, s', w')   -> g w' <$> Strict.runRWST (use a) r s')
    where
      f ((x,_,_),(y,s,w)) = ((x,y),s,w)
      g w (x,s,w') = (x,s,w<>w')

-- | @since 1.0.0.0
instance (Monoid w, MonadMask m) => MonadMask (Strict.RWST r w s m) where
  mask f = Strict.RWST $ \r s -> mask $ \u -> Strict.runRWST (f (q u)) r s
    where
      q :: (forall x. m x -> m x)
        -> Strict.RWST r w s m a -> Strict.RWST r w s m a
      q u (Strict.RWST b) = Strict.RWST $ \r s -> u (b r s)
  uninterruptibleMask f = Strict.RWST $ \r s -> uninterruptibleMask $ \u -> Strict.runRWST (f (q u)) r s
    where
      q :: (forall x. m x -> m x)
        -> Strict.RWST r w s m a -> Strict.RWST r w s m a
      q u (Strict.RWST b) = Strict.RWST $ \r s -> u (b r s)


--
-- Lazy.StateT instances
--

-- | @since 1.0.0.0
instance MonadCatch m => MonadThrow (Lazy.StateT s m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance MonadCatch m => MonadCatch (Lazy.StateT s m) where
  catch (Lazy.StateT m) f = Lazy.StateT $ \s -> catch (m s) (\e -> Lazy.runStateT (f e) s)

  -- | general bracket ignores the state produced by the release callback
  generalBracket acquire release use = Lazy.StateT $ \s -> fmap f $
      generalBracket
        (Lazy.runStateT acquire s)
        (\(resource, s') e ->
          case e of
            ExitCaseSuccess (b, s'') ->
              Lazy.runStateT (release resource (ExitCaseSuccess b)) s''
            ExitCaseException err ->
              Lazy.runStateT (release resource (ExitCaseException err)) s'
            ExitCaseAbort ->
              Lazy.runStateT (release resource ExitCaseAbort) s')
        (\(a, s')   -> Lazy.runStateT (use a) s')
    where f ((x,_),(y,s)) = ((x,y),s)

-- | @since 1.0.0.0
instance MonadMask m => MonadMask (Lazy.StateT s m) where
  mask f = Lazy.StateT $ \s -> mask $ \u -> Lazy.runStateT (f (q u)) s
    where
      q :: (forall x. m x -> m x)
        -> Lazy.StateT s m a -> Lazy.StateT s m a
      q u (Lazy.StateT b) = Lazy.StateT $ \s -> u (b s)
  uninterruptibleMask f = Lazy.StateT $ \s -> uninterruptibleMask $ \u -> Lazy.runStateT (f (q u)) s
    where
      q :: (forall x. m x -> m x)
        -> Lazy.StateT s m a -> Lazy.StateT s m a
      q u (Lazy.StateT b) = Lazy.StateT $ \s -> u (b s)


--
-- Strict.StateT instances
--

-- | @since 1.0.0.0
instance MonadCatch m => MonadThrow (Strict.StateT s m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance MonadCatch m => MonadCatch (Strict.StateT s m) where
  catch (Strict.StateT m) f = Strict.StateT $ \s -> catch (m s) (\e -> Strict.runStateT (f e) s)

  -- | general bracket ignores the state produced by the release callback
  generalBracket acquire release use = Strict.StateT $ \s -> fmap f $
      generalBracket
        (Strict.runStateT acquire s)
        (\(resource, s') e ->
          case e of
            ExitCaseSuccess (b, s'') ->
              Strict.runStateT (release resource (ExitCaseSuccess b)) s''
            ExitCaseException err ->
              Strict.runStateT (release resource (ExitCaseException err)) s'
            ExitCaseAbort ->
              Strict.runStateT (release resource ExitCaseAbort) s')
        (\(a, s')   -> Strict.runStateT (use a) s')
    where f ((x,_),(y,s)) = ((x,y),s)

-- | @since 1.0.0.0
instance MonadMask m => MonadMask (Strict.StateT s m) where
  mask f = Strict.StateT $ \s -> mask $ \u -> Strict.runStateT (f (q u)) s
    where
      q :: (forall x. m x -> m x)
        -> Strict.StateT s m a -> Strict.StateT s m a
      q u (Strict.StateT b) = Strict.StateT $ \s -> u (b s)
  uninterruptibleMask f = Strict.StateT $ \s -> uninterruptibleMask $ \u -> Strict.runStateT (f (q u)) s
    where
      q :: (forall x. m x -> m x)
        -> Strict.StateT s m a -> Strict.StateT s m a
      q u (Strict.StateT b) = Strict.StateT $ \s -> u (b s)

