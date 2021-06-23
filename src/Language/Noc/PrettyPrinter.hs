module Language.Noc.PrettyPrinter where

-------------- Modules -----------------------

import Data.Text (Text, pack, unpack)
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST

-------------  Utils --------------------------

parens :: Text -> Text
parens content = (pack "[") <> content <> (pack "]")

comma :: Text -> Text
comma content = content <> (pack ", ")

---- Expression pretty printer ----

pprintVal :: Value -> Text
pprintVal (FloatVal f) = pack $ show f
pprintVal (IntVal i) = pack $ show i
pprintVal (StringVal s) = pack $ show $ unpack s
pprintVal (BoolVal b) = pack $ show b
pprintVal (QuoteVal l) = parens $ showEnv l

showStack :: Stack -> Text
showStack [] = pack ""
showStack (x : xs) = foldl (\acc x -> (acc <> (pack " ")) <> (pprintVal x)) (pprintVal x) xs

displayStack :: Stack -> String
displayStack s = unpack $ pack "=> " <> (parens $ showStack s)

---- Env pretty printer -------

pprintEnv :: Atom -> Text
pprintEnv (FloatAtom f) = pack $ show f
pprintEnv (IntAtom i) = pack $ show i
pprintEnv (StringAtom s) = pack $ show s
pprintEnv (WordAtom w) = pack w
pprintEnv (BoolAtom b) = pack $ show b
pprintEnv (QuoteAtom l) = parens $ showEnv l

showEnv :: Expr -> Text
showEnv [] = pack ""
showEnv (x : xs) = foldl (\acc x -> (acc <> (pack " ")) <> (pprintEnv x)) (pprintEnv x) xs

displayEnv :: Text -> Expr -> String
displayEnv fname content = unpack $ comma $ fname <> (pack ": ") <> (parens $ showEnv content)

displayQuote :: Expr -> String
displayQuote content = unpack $ (parens $ showEnv content)
