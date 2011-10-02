{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-}
module Lucretia.Exception where
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

{- |
The exception monad transformer
-}
newtype ExceptionT e m a = ExceptionT { runExceptionT :: m (Either e a) }

throwException :: (Monad m) => e -> ExceptionT e m a
throwException = ExceptionT . return . Left

catchException :: (Monad m) => ExceptionT e m a -> (e->ExceptionT e m a) -> ExceptionT e m a
catchException etm h = ExceptionT $ do  
  s <- runExceptionT etm
  case s of
    Left e -> runExceptionT (h e)
    Right a -> (return (Right a))
  -- either (runExceptionT . h) (return . Right) s
  
instance Monad m => Functor (ExceptionT e m) where
  fmap f m = ExceptionT $ do
    s <- runExceptionT m
    either (return.Left) (return.Right . f) s
      
instance Monad m => Monad (ExceptionT e m) where
  return = ExceptionT . return . Right
  m >>= k = ExceptionT $ do
    a <- runExceptionT m
    either (return . Left) (runExceptionT . k) a 

instance MonadTrans (ExceptionT e) where
    lift m = ExceptionT $ do
        a <- m
        return (Right a)
        
instance (MonadError e1 m) => MonadError e1 (ExceptionT e2 m) where
  throwError = ExceptionT .  throwError
  -- ET = ExceptionT
  -- catchError :: (ET e2 m a) -> (e1 -> ET e2 m a) -> ET e2 m a
  catchError etm eth = ExceptionT m where
    -- mh :: e1 -> m (Either e2 a)
    mh e1 = runExceptionT (eth e1)
    -- m :: m (Either e2 a) 
    m = (runExceptionT etm) `catchError` mh

instance (MonadReader r m) => MonadReader r (ExceptionT e m) where
    ask       = lift ask
    local f m = ExceptionT $ local f (runExceptionT m)

instance (MonadState s m) => MonadState s (ExceptionT e m) where
    get = lift get
    put = lift . put