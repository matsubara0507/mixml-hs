{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MixML.EL.Parser.Token where

import           Data.Char                  (chr, toUpper)
import           Data.Functor               (($>), (<$))
import           Data.Maybe                 (fromJust)
import qualified Data.Set                   as Set
import           Text.Megaparsec            (MonadParsec, many, some, (<|>))
import qualified Text.Megaparsec            as P
import           Text.Megaparsec.Char       (alphaNumChar, char, digitChar,
                                             hexDigitChar, letterChar, noneOf,
                                             oneOf, space1, string)
import qualified Text.Megaparsec.Char.Lexer as P

space :: MonadParsec e String m => m ()
space = P.space space1 (P.skipLineComment "(*)") (P.skipBlockComment "(*" "*)")

term :: MonadParsec e String m => String -> m String
term = P.symbol space

symbolChar :: MonadParsec e String m => m Char
symbolChar = oneOf ("-!%&$#+/:<=>?@\\~`|*^" :: String)

alpha, symbol, typevar, ident :: MonadParsec e String m => m String
alpha   = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '\'')
symbol  = some symbolChar
typevar = (:) <$> char '\''  <*> many (alphaNumChar <|> char '_' <|> char '\'')
ident   = P.try $ do
  x <- alpha <|> symbol <|> typevar
  if x `notElem` keywords then
    x <$ space
  else
    P.fancyFailure $ Set.singleton (P.ErrorFail $ x ++ " is keyword")

num :: MonadParsec e String m => m Int
num = (read <$> some digitChar) <* space

text :: MonadParsec e String m => m String
text = P.between (string "\"") (string "\"") (many charParser) <* space

charParser :: MonadParsec e String m => m Char
charParser =
  noneOf ['\"', '\\', '\t', '\n', '\NUL'] <|> (char '\\' *> charParser')
  where
    charParser' = oneOf ['\"', '\\', '/']
              <|> char 'b' $> '\b'
              <|> char 'f' $> '\f'
              <|> char 'n' $> '\n'
              <|> char 'r' $> '\r'
              <|> char 't' $> '\t'
              <|> char 'u' *> (chr . utf2int <$> P.count 4 hexDigitChar)

utf2int :: String -> Int
utf2int = sum . zipWith (*) [4096,256,16,1] . fmap hex2int
  where
    hex2int = fromJust . flip lookup hexis . toUpper
    hexis = zip "0123456789ABCDEF" [0..]

bang_, at_, plus_, cat_, minus_     :: MonadParsec e String m => m String
hash_, lpar_, rpar_, comma_, arrow_ :: MonadParsec e String m => m String
dot_, colon_, seal_, semic_, equal_ :: MonadParsec e String m => m String
iseql_, darrow_, lbrack_, rbrack_   :: MonadParsec e String m => m String
under_, lbrace_, rbrace_, bar_      :: MonadParsec e String m => m String
bang_   = term "!"
at_     = term "@"
plus_   = term "+"
cat_    = term "++"
minus_  = term "-"
hash_   = term "#"
lpar_   = term "("
rpar_   = term ")"
comma_  = term ","
arrow_  = term "->"
dot_    = term "."
colon_  = term ":"
seal_   = term ":>"
semic_  = term ";"
equal_  = term "="
iseql_  = term "=="
darrow_ = term "=>"
lbrack_ = term "["
rbrack_ = term "]"
under_  = term "_"
lbrace_ = term "{"
rbrace_ = term "}"
bar_    = term "|"

bool_, case_, data_, do_, else_, end_    :: MonadParsec e String m => m String
export_, fn_, false_, forall_, fun_, if_ :: MonadParsec e String m => m String
import_, in_, int_, let_, link_, module_ :: MonadParsec e String m => m String
new_, of_, open_, out_, print_, rec_     :: MonadParsec e String m => m String
seals_, signature_, string_, then_       :: MonadParsec e String m => m String
true_, type_, unit_, val_, where_, with_ :: MonadParsec e String m => m String
bool_      = term "bool"
case_      = term "case"
data_      = term "data"
do_        = term "do"
else_      = term "else"
end_       = term "end"
export_    = term "export"
fn_        = term "fn"
false_     = term "false"
forall_    = term "forall"
fun_       = term "fun"
if_        = term "if"
import_    = term "import"
in_        = term "in"
int_       = term "int"
let_       = term "let"
link_      = term "link"
module_    = term "module"
new_       = term "new"
of_        = term "of"
open_      = term "open"
out_       = term "out"
print_     = term "print"
rec_       = term "rec"
seals_     = term "seals"
signature_ = term "signature"
string_    = term "string"
then_      = term "then"
true_      = term "true"
type_      = term "type"
unit_      = term "unit"
val_       = term "val"
where_     = term "where"
with_      = term "with"

keywords :: [String]
keywords =
  [ "!"
  , "@"
  , "+"
  , "++"
  , "-"
  , "#"
  , "("
  , ")"
  , ","
  , "->"
  , "."
  , ":"
  , ":>"
  , ";"
  , "="
  , "=="
  , "=>"
  , "["
  , "]"
  , "_"
  , "{"
  , "}"
  , "|"
  , "bool"
  , "case"
  , "data"
  , "do"
  , "else"
  , "end"
  , "export"
  , "fn"
  , "false"
  , "forall"
  , "fun"
  , "if"
  , "import"
  , "in"
  , "int"
  , "let"
  , "link"
  , "module"
  , "new"
  , "of"
  , "open"
  , "out"
  , "print"
  , "rec"
  , "seals"
  , "signature"
  , "string"
  , "then"
  , "true"
  , "type"
  , "unit"
  , "val"
  , "where"
  , "with"
  ]
