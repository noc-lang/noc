module Language.Noc.PrettyPrinter where

-------------- Modules -----------------------

import Data.Text (Text, pack, unpack)
import Language.Noc.Runtime.Internal
import Language.Noc.Syntax.AST (Declaration (..))

----------------------------------------------

parens :: Text -> Text
parens content = (pack "[") <> content <> (pack "]")

comma :: Text -> Text
comma content = content <> (pack ", ")

pprintValue :: Value -> Text
pprintValue (FloatVal v) = pack $ show v
pprintValue (WordVal v) = pack v
pprintValue (StringVal v) = pack $ show v
pprintValue (QuoteVal l) = parens $ showStack l

showStack :: Stack -> Text
showStack [] = pack ""
showStack (x : xs) = foldl (\acc x -> (acc <> (pack " ")) <> (pprintValue x)) (pprintValue x) xs

displayStack :: Stack -> String
displayStack s = unpack $ pack "=> " <> (parens $ showStack s)

displayEnv :: Text -> Stack -> String
displayEnv fname content = unpack $ comma $ fname <> (pack ": ") <> (parens $ showStack content)
