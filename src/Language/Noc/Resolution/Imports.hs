module Language.Noc.Resolution.Imports where

import Control.Exception (Exception, SomeException, throwIO, try)
import Data.Map (fromList, toList, union, unions)
import Data.Text (Text, unpack)
import Language.Noc.Resolution.Name
import Language.Noc.Syntax.AST
import System.Directory (XdgDirectory (..), getXdgDirectory, listDirectory)
import System.Info (os)
import Text.Parsec (ParseError)

data ImportError = ImportError String deriving (Show)

instance Exception ImportError

spanInternal :: [FilePath] -> ([FilePath], [FilePath]) -> IO ([FilePath], [FilePath])
spanInternal [] (int, noc) = return (int, noc)
spanInternal (path : paths) (int, noc) = case path `elem` (map fst internal) of
  True -> spanInternal paths (int <> [path], noc)
  False -> do
    isSTD <- isSTDModule path
    case isSTD of
      (Just v) -> spanInternal paths (int, noc <> [v])
      Nothing -> spanInternal paths (int, noc <> [path])

putMainFirst :: [(Text,(Maybe DocString, Expr))] -> ([(Text,(Maybe DocString, Expr))], [(Text,(Maybe DocString, Expr))]) -> [(Text,(Maybe DocString, Expr))]
putMainFirst [] (m,o) = m <> o
putMainFirst (x:xs) (m,o) = case unpack $ fst x of
  "main" -> putMainFirst xs ([x], o)
  _ -> putMainFirst xs (m, o <> [x])

isSTDModule :: FilePath -> IO (Maybe String)
isSTDModule p = do
  case (take 4 p) == "std:" of
    True -> do
      let file = (drop 4 p) <> ".noc"
      stdPath <- getXdgDirectory XdgData (if os == "mingw32" then "local/noc/std" else "noc/std")
      tryStdFiles <- try (listDirectory stdPath) :: IO (Either SomeException [FilePath])
      case tryStdFiles of
        (Left err) -> do
          isInSTD <- listDirectory "/app/std/"
          case file `elem` isInSTD of
            True -> return $ Just $ "/app/std/" <> file -- STD path to host Noc in Heroku
            False -> return Nothing
        (Right succ) -> case file `elem` succ of
          True -> return $ Just $ stdPath <> "/" <> file
          False -> return Nothing
    False -> return Nothing
  
allNocFuncs :: String -> [(FilePath, [String])] -> Maybe String
allNocFuncs func [] = Just func
allNocFuncs func ((path, funcs) : xs) = case func `elem` funcs of
  True -> Nothing
  False -> allNocFuncs func xs

checkFunction :: [FilePath] -> Functions -> [(FilePath, [String])] -> Atom -> Maybe String
checkFunction int mainFuncs paths atom = case atom of
  (WordAtom w) -> case (any (== w) prelude) || (any (== w) opcodes') || (any (elem w . maybe [] id . flip lookup internal) int) of
    True -> Nothing
    False -> case w `elem` (map (unpack . fst) (toList mainFuncs)) of
      True -> Nothing
      False -> Just w
  _ -> Nothing

allScope :: [FilePath] -> Functions -> [(FilePath, [String])] -> Expr -> Maybe String
allScope _ _ _ [] = Nothing
allScope int mainFuncs [] (x : xs) = case checkFunction int mainFuncs [] x of
  (Just v) -> Just v
  Nothing -> allScope int mainFuncs [] xs
allScope int mainFuncs paths (x : xs) = case checkFunction int mainFuncs paths x of
  (Just v) -> case allNocFuncs v paths of
    (Just v') -> Just v'
    Nothing -> allScope int mainFuncs paths xs
  Nothing -> allScope int mainFuncs paths xs

scope :: Module -> [FilePath] -> [(FilePath, [String])] -> Either String [()]
scope m int impFuncs = traverse output $ runScope m int impFuncs
  where
    runScope m int impFuncs = map (\(name, (_, funcs)) -> (unpack name, allScope int (decls m) impFuncs funcs)) (toList $ decls m)
    output (name, scope') = case scope' of
      (Just v) -> Left $ (main m) <> ": '" <> v <> "' function is not defined (check '" <> name <> "' function declaration)."
      Nothing -> Right ()

parseImports :: [Module] -> Functions -> IO (Either ParseError [(Text,(Maybe DocString, Expr))])
parseImports [] funcs = return $ Right $ putMainFirst (toList funcs) ([],[])
parseImports (m : ms) funcs = do
  (int, nocFiles) <- spanInternal (imports m) ([], [])
  p <- traverse parseNocFile nocFiles
  let parsed = traverse id p
  case parsed of
    (Left err) -> return $ Left err
    (Right succ) -> do
      let impFuncs = zip nocFiles $ map (\x -> map (unpack . fst) $ toList $ decls x) succ
      case scope m int impFuncs of
        (Left err) -> throwIO $ ImportError err
        (Right _) -> parseImports (ms <> succ) (funcs <> (unions $ map decls succ))