{-# LANGUAGE DeriveFunctor #-}
import           Control.Applicative (Alternative, empty, (<|>), (<*>))
import           Control.Monad       (MonadPlus, mzero)
import           Control.Monad.Trans (MonadTrans, MonadIO, lift, liftIO)

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  } deriving (Functor)

bindMT
  :: Monad m
  => MaybeT m a
  -> (a -> MaybeT m b)
  -> MaybeT m b
bindMT m f = MaybeT $
  runMaybeT m
  >>= maybe (return Nothing) (runMaybeT . f)

apMT
  :: Applicative m
  => MaybeT m (a -> b)
  -> MaybeT m a
  -> MaybeT m b
apMT f a = MaybeT $
  go <$> runMaybeT f <*> runMaybeT a
  where
    go :: Maybe (a -> b) -> Maybe a -> Maybe b
    go mf ma =
      maybe Nothing (\f' -> maybe Nothing (Just . f') ma ) mf

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  (<*>) = apMT

instance Monad m => Monad (MaybeT m) where
  (>>=) = bindMT

instance MonadTrans MaybeT where
  lift m = MaybeT (Just <$> m)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO m = MaybeT $ liftIO m >>= (return . Just)

instance Alternative f => Alternative (MaybeT f) where
  empty = MaybeT (pure Nothing)
  fa <|> fb = MaybeT (runMaybeT fa <|> runMaybeT fb)

instance MonadPlus m => MonadPlus (MaybeT m) where
  mzero = MaybeT (return Nothing)

prompt
    :: String
    -> MaybeT IO String
prompt s = do
    l <- liftIO $ do
        putStr s
        getLine
    if l == "END"
       then mzero -- XXX or empty?
       else return l

program :: MaybeT IO (String, String)
program = do
    a <- prompt "firstname: "
    b <- prompt "lastname: "
    return (a, b)

main :: IO ()
main = do
  runMaybeT program >>= print
