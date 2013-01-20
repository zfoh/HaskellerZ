import Data.ByteString.Char8 (pack)
import Data.Generics -- (everywhere, mkT, everything, mkQ, extQ, everywhereBut)
import Language.C.Data.Position (position)
import Language.C.Parser (parseC)
import Language.C.Pretty (pretty)
import Language.C.Syntax.AST
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Syntax.Constants
import qualified Data.Map as M

import Control.Applicative
import Control.Monad.State (evalState, runState, State, get, put)

parse :: String -> CTranslUnit
parse code = case parseC code' pos of 
  Left e -> error $ show e     
  Right ast -> ast             
  where 
    code' = pack code          
    pos = position 0 "<<test>>" 0 0 

code = parse $ concat [
  "char f[] = \"asdf\";",
  "void main() {",
  "int k = 23;",
  "k = 23;",
  "return 23;",
  "}",
  "void main() {",
  "int k = 13;",
  "k = 21;",
  "return 21;",
  "}"
  ]

un = undefNode

ident :: String -> Ident -- {{{1
ident s = Ident s 0 un

collect :: CTranslUnit -> [String]
collect ast = everything (++) (mkQ [] queryDecl `extQ` queryStat) ast
  where
  queryDecl :: CDecl -> [String]
  queryDecl o@(CDecl [CTypeSpec (CIntType _)] _ _) = [show $ pretty o]
  queryDecl _ = []
  
  queryStat :: CStat -> [String]
  queryStat o@(CReturn _ _) = [show $ pretty o]
  queryStat _ = []

rewrite :: CTranslUnit -> CTranslUnit
rewrite ast = everywhereBut (mkQ False quit) (mkT trans) ast
  where
  trans :: CDecl -> CDecl
  trans _ = CDecl [CTypeSpec (CIntType undefNode)] [(Just (CDeclr (Just (ident "i")) [] Nothing [] un), Nothing, Nothing)] un

  quit :: CExtDecl -> Bool
  quit (CFDefExt _) = True
  quit _ = False

perform :: CTranslUnit -> CTranslUnit
perform ast = runMangle $ everywhereM (mkM trans) ast
  where
  trans :: Ident -> Mangled Ident
  trans (Ident s a b) = Ident <$> mangle s <*> pure a <*> pure b

--  trans (CString x b) = CString (x ++ x) b

type Mangled = State (Int, M.Map String String)

mangle :: String -> Mangled String
mangle name0 = do
    (counter, dict) <- get
    case M.lookup name0 dict of
      Nothing   -> do let name = name0 ++ show counter
                      put (counter + 1, M.insert name0 name dict)
                      return name
      Just name -> return name

runMangle :: Mangled a -> a
runMangle m = evalState m (0, M.empty)

main = do
  print $ pretty code
  print "<<<<<<<<<<<<<<<<<<<<<<<<"
  -- print $ pretty $ rewrite code
  print $ pretty $ perform code
--  print $ collect code
