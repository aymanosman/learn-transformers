{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Control.Monad          (liftM, when)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (MonadState, StateT, get, modify,
                                         runStateT)
import           Control.Monad.Trans    (lift, liftIO)
import           Control.Monad.Writer   (MonadWriter, WriterT, execWriterT,
                                         runWriterT, tell)
import           Data.Bifunctor         (bimap)
import           Data.Foldable          (for_)
import           Data.List              (partition)
import           Data.Traversable       (for)
import           System.Directory       (doesDirectoryExist,
                                         getDirectoryContents)
import           System.FilePath        ((</>))

import           Test.Hspec             (hspec, it, shouldBe)

main ::IO ()
main = do
  putStrLn "--- Normal ---"
  countEntries "a" >>= print
  putStrLn "\n--- Using WriterT ---"
  execWriterT (countEntriesW "a") >>= print
  putStrLn "\n--- With max depth (simple) ---"
  countEntriesToDepth 2 "a" >>= print

  putStrLn "\n--- Using ReaderT and StateT"
  let printer (xs, m) = print xs >> print m
  runRS (Config 2) (countEntriesToDepthRS "a") >>= printer
  runRS (Config 99) (countEntriesToDepthRS "a") >>= printer

  putStrLn "\n--- Using ReaderT, WriterT and StateT ---"
  let printer (_, xs, m) = print xs >> print m
  runRWS (Config 2) (countEntriesToDepthRWS "a") >>= printer
  runRWS (Config 99) (countEntriesToDepthRWS "a") >>= printer

  putStrLn "\n--- Using newtype ---"
  run (Config 2) (countEntriesM "a") >>= printer
  run (Config 99) (countEntriesM "a") >>= printer

  tests



listDirectory :: FilePath -> IO [String]
listDirectory =
    liftM (filter notDots) . getDirectoryContents

notDots :: FilePath -> Bool
notDots p = p /= "." && p /= ".."


-- Normal

countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries path = do
    contents <- listDirectory path
    rest <- for contents $ \path' -> do
              let newPath = path </> path'
              isDir <- doesDirectoryExist newPath
              if isDir
                then countEntries newPath
                else return []
    return $ (path, length contents) : concat rest


-- Using WriterT

countEntriesW :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntriesW path = do
  contents <- liftIO $ listDirectory path
  tell [(path, length contents)]
  for_ contents $ \path' -> do
    let newPath = path </> path'
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir
      then countEntriesW newPath
      else return ()


-- With max depth (simple)

data DirectoryContent
  = ContentCount Int
  | NotDirectory
  | NotCalculated
  deriving (Eq)

instance Show DirectoryContent where
  show (ContentCount n) = show n
  show NotDirectory     = "<none>"
  show NotCalculated    = "?"

countEntriesToDepth
  :: Int
  -> FilePath
  -> IO [(FilePath, DirectoryContent)]
countEntriesToDepth toDepth = go 0
  where
    go :: Int -> FilePath -> IO [(FilePath, DirectoryContent)]
    go depth path | depth == toDepth = return [(path, NotCalculated)]
                  | otherwise = do
      contents <- listDirectory path
      (dirs, _normal) <- partitionContents $ map (path </>) contents
      rest <- mapM (go (depth+1)) dirs
      return $
        (path, ContentCount $ length contents)
        -- : (map (\p -> (depth+1, p, NotDirectory)) normal)
        : concat rest

partitionContents
  :: [FilePath]
  -> IO ([FilePath], [FilePath]) -- (dirs, normal)
partitionContents paths = do
  xs <- mapM doesDirectoryExist paths
  return $ bimap (map snd) (map snd) $ partition fst $ zip xs paths


-- Using ReaderT and StateT

data Config = Config
  { configMaxDepth :: Int
  } deriving (Show)

data Model = Model
  { stateDeepestReached :: Int
  } deriving (Eq, Show)

type ProgramRS a = ReaderT Config (StateT Model IO) a

runRS :: Config -> ProgramRS a -> IO (a, Model)
runRS config prog =
  runStateT
    (runReaderT prog config)
    (Model 0)

countEntriesToDepthRS
  :: FilePath
  -> ProgramRS [(FilePath, DirectoryContent)]
countEntriesToDepthRS = go 0
  where
    go :: Int -> FilePath -> ProgramRS [(FilePath, DirectoryContent)]
    go depth path = do
      maxDepth <- configMaxDepth <$> ask
      if depth == maxDepth
        then return [(path, NotCalculated)]
        else do
          deepest <- stateDeepestReached <$> get
          when (depth >= deepest) $
            modify incrementDeepest
          contents <- liftIO $ listDirectory path
          (dirs, _normal) <- liftIO $ partitionContents $ map (path </>) contents
          rest <- mapM (go (depth+1)) dirs
          return $
            (path, ContentCount $ length contents)
            : concat rest

incrementDeepest :: Model -> Model
incrementDeepest s =
  s {stateDeepestReached = stateDeepestReached s + 1}


-- Using ReaderT, WriterT and StateT

type ProgramRWS a =
  ReaderT Config
  (WriterT [(FilePath, DirectoryContent)]
   (StateT Model IO)) a

runRWS
  :: Config
  -> ProgramRWS a
  -> IO (a, [(FilePath, DirectoryContent)], Model)
runRWS config prog = do
  ((a, w), m) <- runStateT (runWriterT (runReaderT prog config)) (Model 0)
  return (a, w, m)

countEntriesToDepthRWS
  :: FilePath
  -> ProgramRWS ()
countEntriesToDepthRWS = go 0
  where
    go :: Int -> FilePath -> ProgramRWS ()
    go depth path = do
      maxDepth <- configMaxDepth <$> ask
      if depth == maxDepth
        then lift $ tell [(path, NotCalculated)]
        else do
          deepest <- stateDeepestReached <$> get
          when (depth >= deepest) $
            modify incrementDeepest
          contents <- liftIO $ listDirectory path
          lift $ tell [(path, ContentCount $ length contents)]
          for_ contents $ \path' -> do
            let newPath = path </> path'
            isDir <- liftIO $ doesDirectoryExist newPath
            when isDir $
              go (depth+1) newPath


-- Using newtype

type LogCountEntries = [(FilePath, DirectoryContent)]

newtype Program a = Program
  { runProgram :: ReaderT Config
                    (WriterT LogCountEntries
                     (StateT Model IO)) a
  }
  deriving (Functor, Applicative, Monad, MonadIO,
            MonadReader Config,
            MonadWriter LogCountEntries,
            MonadState Model)

run
  :: Config
  -> Program a
  -> IO (a, LogCountEntries, Model)
run config prog = do
  ((a, w), m) <- runStateT (runWriterT (runReaderT (runProgram prog) config)) (Model 0)
  return (a, w, m)

countEntriesM
  :: FilePath
  -> Program ()
countEntriesM = go 0
  where
    go :: Int -> FilePath -> Program ()
    go depth path = do
      maxDepth <- configMaxDepth <$> ask
      if depth == maxDepth
        then tell [(path, NotCalculated)]
        else do
          deepest <- stateDeepestReached <$> get
          when (depth >= deepest) $
            modify incrementDeepest
          contents <- liftIO $ listDirectory path
          tell [(path, ContentCount $ length contents)]
          for_ contents $ \path' -> do
            let newPath = path </> path'
            isDir <- liftIO $ doesDirectoryExist newPath
            when isDir $
              go (depth+1) newPath


-- Tests

tests :: IO ()
tests = hspec $ do
 it "they are equivalent" $ do
   (a, _) <-    runRS  (Config 99) (countEntriesToDepthRS "a")
   (_, b, _) <- runRWS (Config 99) (countEntriesToDepthRWS "a")
   a `shouldBe` b

 it "they are equivalent" $ do
   b <- runRWS (Config 99) (countEntriesToDepthRWS "a")
   c <- run    (Config 99) (countEntriesM "a")
   b `shouldBe` c
