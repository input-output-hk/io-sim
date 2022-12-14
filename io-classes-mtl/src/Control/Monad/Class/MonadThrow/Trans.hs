{-# LANGUAGE RankNTypes      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Control.Monad.Class.MonadThrow.Trans () where

import           Control.Monad.Except
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Writer

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
-- WriterT instances
--

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadThrow (WriterT w m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadCatch (WriterT w m) where
  catch (WriterT m) f = WriterT $ catch m (runWriterT . f)

  generalBracket acquire release use = WriterT $ fmap f $
      generalBracket
        (runWriterT acquire)
        (\(resource, w) e ->
          case e of
            ExitCaseSuccess (b, w') ->
              g w' <$> runWriterT (release resource (ExitCaseSuccess b)) 
            ExitCaseException err ->
              g w  <$> runWriterT (release resource (ExitCaseException err))
            ExitCaseAbort ->
              g w  <$> runWriterT (release resource ExitCaseAbort))
        (\(resource, w)   -> g w <$> runWriterT (use resource))
    where f ((x,_),(y,w)) = ((x,y),w)
          g w (a,w') = (a,w<>w')

-- | @since 1.0.0.0
instance (Monoid w, MonadMask m) => MonadMask (WriterT w m) where
  mask f = WriterT $ mask $ \u -> runWriterT $ f (q u)
    where
      q :: (forall x. m x -> m x)
        -> WriterT w m a -> WriterT w m a
      q u (WriterT b) = WriterT (u b)
  uninterruptibleMask f = WriterT $ uninterruptibleMask $ \u -> runWriterT $ f (q u)
    where
      q :: (forall x. m x -> m x)
        -> WriterT w m a -> WriterT w m a
      q u (WriterT b) = WriterT (u b)


--
-- RWST Instances
--

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadThrow (RWST r w s m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance (Monoid w, MonadCatch m) => MonadCatch (RWST r w s m) where
  catch (RWST m) f = RWST $ \r s -> catch (m r s) (\e -> runRWST (f e) r s)

  -- | general bracket ignores the state produced by the release callback
  generalBracket acquire release use = RWST $ \r s ->
      f <$> generalBracket
              (runRWST acquire r s)
              (\(resource, s', w') e ->
                case e of
                  ExitCaseSuccess (b, s'', w'') ->
                    g w'' <$> runRWST (release resource (ExitCaseSuccess b)) r s''
                  ExitCaseException err ->
                    g w'  <$> runRWST (release resource (ExitCaseException err)) r s'
                  ExitCaseAbort ->
                    g w'  <$> runRWST (release resource  ExitCaseAbort) r s')
              (\(a, s', w')   -> g w' <$> runRWST (use a) r s')
    where
      f ((x,_,_),(y,s,w)) = ((x,y),s,w)
      g w (x,s,w') = (x,s,w<>w')

-- | @since 1.0.0.0
instance (Monoid w, MonadMask m) => MonadMask (RWST r w s m) where
  mask f = RWST $ \r s -> mask $ \u -> runRWST (f (q u)) r s
    where
      q :: (forall x. m x -> m x)
        -> RWST r w s m a -> RWST r w s m a
      q u (RWST b) = RWST $ \r s -> u (b r s)
  uninterruptibleMask f = RWST $ \r s -> uninterruptibleMask $ \u -> runRWST (f (q u)) r s
    where
      q :: (forall x. m x -> m x)
        -> RWST r w s m a -> RWST r w s m a
      q u (RWST b) = RWST $ \r s -> u (b r s)

--
-- StateT instances
--

-- | @since 1.0.0.0
instance MonadCatch m => MonadThrow (StateT s m) where
  throwIO = lift . throwIO

-- | @since 1.0.0.0
instance MonadCatch m => MonadCatch (StateT s m) where
  catch (StateT m) f = StateT $ \s -> catch (m s) (\e -> runStateT (f e) s)

  -- | general bracket ignores the state produced by the release callback
  generalBracket acquire release use = StateT $ \s -> fmap f $
      generalBracket
        (runStateT acquire s)
        (\(resource, s') e ->
          case e of
            ExitCaseSuccess (b, s'') ->
              runStateT (release resource (ExitCaseSuccess b)) s''
            ExitCaseException err ->
              runStateT (release resource (ExitCaseException err)) s'
            ExitCaseAbort ->
              runStateT (release resource ExitCaseAbort) s')
        (\(a, s')   -> runStateT (use a) s')
    where f ((x,_),(y,s)) = ((x,y),s)

-- | @since 1.0.0.0
instance MonadMask m => MonadMask (StateT s m) where
  mask f = StateT $ \s -> mask $ \u -> runStateT (f (q u)) s
    where
      q :: (forall x. m x -> m x)
        -> StateT s m a -> StateT s m a
      q u (StateT b) = StateT $ \s -> u (b s)
  uninterruptibleMask f = StateT $ \s -> uninterruptibleMask $ \u -> runStateT (f (q u)) s
    where
      q :: (forall x. m x -> m x)
        -> StateT s m a -> StateT s m a
      q u (StateT b) = StateT $ \s -> u (b s)

