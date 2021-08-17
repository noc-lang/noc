module Interpreter.Utils where

import Control.Monad.Except
import Control.Monad.RWS
import qualified Data.Map as M (Map, empty, fromList, keys, lookup, toList, union)
import qualified Data.Text as T (Text, empty, pack, unpack, unwords)
import Language.Noc.Runtime.Eval (evalFile)
import Language.Noc.Runtime.Internal
import Language.Noc.Runtime.Prelude (otherModules, prelude)
import Language.Noc.Syntax.AST
import System.Directory (XdgDirectory (..), getXdgDirectory, listDirectory, doesPathExist)
import System.Info (os)
import Control.Exception (try, SomeException)
import qualified Text.Parsec.String as P (parseFromFile)

----------------------------------------------

filterProg :: [Env] -> ([(T.Text, (Maybe DocString, Expr))], Env)
filterProg prog = (main', other')
  where
    filter' pred ast = M.fromList $ filter (\(k, _) -> pred k (T.pack "main")) (M.toList $ foldr M.union M.empty ast)
    main' = map (\(k, (Function v)) -> (k, v)) (M.toList $ filter' (==) prog)
    other' = filter' (/=) prog

isInternalModule :: FilePath -> (Maybe Env, Bool)
isInternalModule p = case M.lookup (T.pack p) otherModules of
  Nothing -> (Nothing, False)
  decls -> (decls, True)

isSTDModule :: FilePath -> IO (Maybe String)
isSTDModule p = do
  let file = (drop 4 p) <> ".noc"
  stdPath <- getXdgDirectory XdgData (if os == "mingw32" then "local/noc/std" else "noc/std")
  tryStdFiles <- try (listDirectory stdPath) :: IO (Either SomeException [FilePath])
  case tryStdFiles of
    (Left err) -> do
      isInSTD <- listDirectory "/app/std/"
      case file `elem` isInSTD of
        True -> return $ Just $ "/app/std/" <> file  -- STD path to host Noc in Heroku
        False -> return Nothing
    (Right succ) -> do
      case (take 4 p == "std:") && (file `elem` succ) of
        True -> return $ Just $ stdPath <> "/" <> file
        False -> return Nothing

allMaybe :: (a -> Maybe String) -> [a] -> Maybe String
allMaybe f [] = Nothing
allMaybe f (x : xs) = case f x of
  Just k -> Just k
  Nothing -> allMaybe f xs

checkPath :: FilePath -> IO FilePath
checkPath p = do
  isSTDModule' <- isSTDModule p
  case isSTDModule' of
    (Just path) -> return path
    Nothing -> return p

------------ Scope functions ------------------

isNocFunction :: Atom -> Bool
isNocFunction (WordAtom n) = not $ n `elem` (map (T.unpack . fst) (M.toList prelude))
isNocFunction _ = False

checkScopeFuncs :: [String] -> ([String], [String]) -> [String] -> Maybe String
checkScopeFuncs [] _ _ = Nothing
checkScopeFuncs (x : xs) (l1, l2) currentImp = case (x `elem` l1) || (x `elem` l2) of
  True -> checkScopeFuncs xs (l1, l2) currentImp
  False -> case (any (\y -> y `elem` (map T.unpack $ M.keys otherModules)) currentImp) && x `elem` internalFuncsName of
    True -> checkScopeFuncs xs (l1, l2) currentImp
    False -> Just x
  where
    internalFuncsName = map T.unpack $ M.keys $ foldr M.union M.empty $ map snd (M.toList otherModules)

scopeModule :: [([FilePath], Env)] -> Env -> [String] -> (T.Text, EnvEntry) -> Maybe String
scopeModule imports e currentImp (_, Function (_, decls)) = case importFuncNames of
  [] -> case funcNames of
    [] -> Nothing
    _ -> checkScopeFuncs funcNames (currentFileFuncs, importFuncNames) currentImp
  _ -> checkScopeFuncs funcNames (currentFileFuncs, importFuncNames) currentImp
  where
    names y = map (\(WordAtom x) -> x) $ filter isNocFunction y
    funcNames = names decls
    currentFileFuncs = map T.unpack $ M.keys e
    importFuncNames = map T.unpack $ foldr (<>) [] $ map (M.keys . snd) imports

------------- Parse Noc File ---------------

parseNocFile :: FilePath -> IO (Either String ([FilePath], Env))
parseNocFile p = do
  parse <- (checkPath p) >>= P.parseFromFile program
  case parse of
    (Left err) -> return $ Left $ show err
    (Right (Module imports decls)) -> return $ Right (imports, M.fromList $ map (\(k, d) -> (k, Function d)) (M.toList $ foldr M.union M.empty decls))

parseImports :: [FilePath] -> [Env] -> IO (Either String [Env])
parseImports [] imported = return $ Right imported
parseImports (p : ps) imported = case isInternalModule p of
  (Just decls, _) -> parseImports ps (imported <> [decls])
  (Nothing, _) -> do
    parsed <- parseNocFile p
    case parsed of
      (Left err) -> return $ Left err
      (Right (imports, env')) -> do
        v <- traverse parseNocFile (filter (not . snd . isInternalModule) imports)
        let v' = traverse id v
        case v' of
          (Left err) -> return $ Left err
          (Right succ) -> do
            let pred = allMaybe (scopeModule succ env' imports) (M.toList env')
            case pred of
              Just w -> return $ Left $ p <> ": the '" <> w <> "' function is unknown."
              Nothing -> parseImports (imports <> ps) (imported <> [env'])

------------ Run Noc file --------------------------

runModule :: FilePath -> IO (Maybe Stack)
runModule path = do
  parsed <- parseImports [path] []
  case parsed of
    (Right succ) -> do
      let (main', other') = filterProg succ
      let mainEnv = M.fromList $ map (\(a,b) -> (a, Function b)) main'
      evalFile' <- runExceptT $ runRWST (evalFile $ main') (prelude <> mainEnv <> other') []
      case evalFile' of
        (Left err') -> (print err') >> return Nothing
        (Right (_, s, _)) -> return $ Just s
    (Left err) -> putStrLn err >> return Nothing
