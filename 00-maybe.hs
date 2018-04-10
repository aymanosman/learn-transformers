import           Control.Monad             (mzero)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe


main :: IO ()
main =
    -- program >>= print
    run programT >>= print


-- Without Transformers

prompt
    :: String
    -> IO (Maybe String)
program
    :: IO (Maybe (String, String))

prompt s = do
    putStr s
    l <- getLine
    if l == "END"
       then return Nothing
       else return $ Just l

program = do
    a <- prompt "firstname: "
    case a of
      Nothing -> return Nothing
      Just a' -> do
        b <- prompt "lastname: "
        case b of
          Nothing -> return Nothing
          Just b' -> return $ Just (a', b')


-- Using Transformers

type Prog = MaybeT IO

run :: Prog a -> IO (Maybe a)
run = runMaybeT

promptT
    :: String
    -> Prog String
programT
    :: Prog (String, String)

promptT s = do
    l <- liftIO $ do
        putStr s
        getLine
    if l == "END"
       then mzero
       else return l

programT = do
    a <- promptT "firstname: "
    b <- promptT "lastname: "
    return (a, b)
