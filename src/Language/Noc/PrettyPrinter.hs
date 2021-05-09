module Language.Noc.PrettyPrinter where

-------------- Modules -----------------------

import Data.Text (Text, pack, unpack)
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST

----------------------------------------------

parens :: Text -> Text
parens content = (pack "[") <> content <> (pack "]")

comma :: Text -> Text
comma content = content <> (pack ", ")

---- Expression pretty printer ----

pprintVal :: Value -> Text
pprintVal (FloatVal f) = pack $ show f
pprintVal (StringVal s) = pack $ show s
pprintVal (QuoteVal l) = parens $ showEnv l

showStack :: Stack -> Text
showStack [] = pack ""
showStack (x : xs) = foldl (\acc x -> (acc <> (pack " ")) <> (pprintVal x)) (pprintVal x) xs

displayStack :: Stack -> String
displayStack s = unpack $ pack "=> " <> (parens $ showStack s)

---- Env pretty printer -------

pprintEnv :: Atom -> Text
pprintEnv (FloatAtom f) = pack $ show f
pprintEnv (StringAtom s) = pack $ show s
pprintEnv (WordAtom w) = pack w
pprintEnv (QuoteAtom l) = parens $ showEnv l

showEnv :: Expr -> Text
showEnv [] = pack ""
showEnv (x : xs) = foldl (\acc x -> (acc <> (pack " ")) <> (pprintEnv x)) (pprintEnv x) xs

displayEnv :: Text -> Expr -> String
displayEnv fname content = unpack $ comma $ fname <> (pack ": ") <> (parens $ showEnv content)
