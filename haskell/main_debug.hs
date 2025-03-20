{- README.md -}

{-# LANGUAGE MultilineStrings #-}

-- This is like main.hs, but includes a step-by-step debugger.

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (chr, ord)
import Data.IORef
import Data.Word
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec hiding (State)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Text.Parsec as Parsec
import Data.Maybe (isJust)

type Name = Word64

data Term
  = Var Name                     -- Name
  | Let Name Term Term           -- "! " Name " = " Term "; " Term
  | Era                          -- "*"
  | Sup Name Term Term           -- "&" Name "{" Term "," Term "}"
  | Dup Name Name Name Term Term -- "! &" Name "{" Name "," Name "}" "=" Term ";" Term
  | Lam Name Name Term           -- "&" Label "λ" Name "." Term
  | App Name Term Term           -- "&" Label "(" Term " " Term ")"

-- Globals
-- -------

{-# NOINLINE gSUBST #-}
gSUBST :: IORef (IntMap.IntMap Term)
gSUBST = unsafePerformIO $ newIORef IntMap.empty

{-# NOINLINE gFRESH #-}
gFRESH :: IORef Name
gFRESH = unsafePerformIO $ newIORef 0

{-# NOINLINE gINTERS #-}
gINTERS :: IORef Word64
gINTERS = unsafePerformIO $ newIORef 0

{-# NOINLINE gSTOP #-}
gSTOP :: IORef Bool
gSTOP = unsafePerformIO $ newIORef False

-- Helper functions for global substitution
set :: Name -> Term -> IO ()
set name term = do
  subMap <- readIORef gSUBST
  writeIORef gSUBST (IntMap.insert (fromIntegral name) term subMap)

get :: Name -> IO (Maybe Term)
get name = do
  subMap <- readIORef gSUBST
  let result = IntMap.lookup (fromIntegral name) subMap
  when (isJust result) $ do
    let newMap = IntMap.delete (fromIntegral name) subMap
    writeIORef gSUBST newMap
  return result

fresh :: IO Name
fresh = do
  n <- readIORef gFRESH
  writeIORef gFRESH (n + 1)
  return n

incInters :: IO ()
incInters = do
  n <- readIORef gINTERS
  writeIORef gINTERS (n + 1)

markReduction :: IO ()
markReduction = writeIORef gSTOP True

hasReduced :: IO Bool
hasReduced = readIORef gSTOP

resetReduction :: IO ()
resetReduction = writeIORef gSTOP False

-- Function to display the substitution map
showSubst :: IO String
showSubst = do
  subMap <- readIORef gSUBST
  if IntMap.null subMap
    then return ""
    else do
      let entries = IntMap.toList subMap
      let showEntry (k, v) = name (fromIntegral k) ++ " <- " ++ show v
      return $ unlines (map showEntry entries)

-- Evaluator
-- ---------

app_era :: Term -> Term -> IO Term
app_era Era _ = do
  incInters
  markReduction
  return Era
app_era _ _ = error "app_era: expected Era as first argument"

app_lam :: Term -> Term -> Name -> IO Term
app_lam (Lam lam_lab nam bod) arg app_lab = do
  incInters
  markReduction
  if lam_lab == app_lab then do
    set nam arg
    return bod
  else do
    y <- fresh
    z <- fresh
    f <- fresh
    x <- fresh
    v <- fresh
    set nam (Lam app_lab y (Var z))
    return $ 
      (Let f bod
      (Let x (App lam_lab arg (Var y))
      (Let v (App app_lab (Var f) (Var x))
      (Lam lam_lab z (Var v)))))
app_lam _ _ _ = error "app_lam: expected Lam as first argument"

app_sup :: Term -> Term -> Name -> IO Term
app_sup (Sup lab lft rgt) arg app_lab = do
  incInters
  markReduction
  c0 <- fresh
  c1 <- fresh
  let a0 = App app_lab lft (Var c0)
  let a1 = App app_lab rgt (Var c1)
  return (Dup lab c0 c1 arg (Sup lab a0 a1))
app_sup _ _ _ = error "app_sup: expected Sup as first argument"

app_dup :: Term -> IO Term
app_dup (App app_lab f (Dup dup_lab x y val bod)) = do
  incInters
  markReduction
  return (Dup dup_lab x y val (App app_lab f bod))
app_dup term = error "app_dup: expected App with Dup"

dup_era :: Term -> Term -> IO Term
dup_era (Dup lab r s _ k) Era = do
  incInters
  markReduction
  set r Era
  set s Era
  return k
dup_era _ _ = error "dup_era: expected Dup and Era"

dup_lam :: Term -> Term -> IO Term
dup_lam (Dup lab r s _ k) (Lam lam_lab x f) = do
  incInters
  markReduction
  x0 <- fresh
  x1 <- fresh
  f0 <- fresh
  f1 <- fresh
  set r (Lam lam_lab x0 (Var f0))
  set s (Lam lam_lab x1 (Var f1))
  set x (Sup lab (Var x0) (Var x1))
  return (Dup lab f0 f1 f k)
dup_lam _ _ = error "dup_lam: expected Dup and Lam"

dup_sup :: Term -> Term -> IO Term
dup_sup (Dup dupLab x y _ k) (Sup supLab a b) = do
  incInters
  markReduction
  if dupLab == supLab then do
    set x a
    set y b
    return k
  else do
    a0 <- fresh
    a1 <- fresh
    b0 <- fresh
    b1 <- fresh
    set x (Sup supLab (Var a0) (Var b0))
    set y (Sup supLab (Var a1) (Var b1))
    return (Dup dupLab a0 a1 a (Dup dupLab b0 b1 b k))
dup_sup _ _ = error "dup_sup: expected Dup and Sup"

dup_dup :: Term -> Term -> IO Term
dup_dup (Dup labL x0 x1 _ t) (Dup labR y0 y1 y x) = do
  incInters
  markReduction
  return (Dup labL x0 x1 x (Dup labL y0 y1 y t))
dup_dup _ _ = error "dup_dup: expected Dup with inner Dup"

whnf :: Term -> IO Term
whnf term = case term of
  Var n -> do
    sub <- get n
    case sub of
      Just s -> do
        markReduction
        whnf s
      Nothing -> return (Var n)
  Let x v b -> do
    print "LET"
    v' <- whnf v
    didReduce <- hasReduced
    if didReduce then
      return (Let x v' b)
    else do
      set x v'
      markReduction
      whnf b
  App app_lab f a -> do
    f' <- whnf f
    didReduce <- hasReduced
    if didReduce then
      return (App app_lab f' a)
    else do
      print "APP"
      case f' of
        Lam {} -> app_lam f' a app_lab
        Sup {} -> app_sup f' a app_lab
        Era    -> app_era f' a
        Dup {} -> app_dup (App app_lab f' a)
        _      -> return (App app_lab f' a)
  Dup dup_lab r s v k -> do
    print "DUP"
    v' <- whnf v
    didReduce <- hasReduced
    if didReduce then
      return (Dup dup_lab r s v' k)
    else
      case v' of
        Lam {} -> dup_lam (Dup dup_lab r s v' k) v'
        Sup {} -> dup_sup (Dup dup_lab r s v' k) v'
        Era    -> dup_era (Dup dup_lab r s v' k) v'
        Dup {} -> dup_dup (Dup dup_lab r s v' k) v'
        _      -> return (Dup dup_lab r s v' k)
  _ -> return term

-- FIXME: this is ugly, improve
step :: Term -> IO Term
step term = do
  resetReduction
  term' <- whnf term
  didReduce <- hasReduced
  if didReduce then
    return term'
  else do
    resetReduction
    case term' of
      Lam lam_lab x b -> do
        b' <- step b
        didReduce <- hasReduced
        if didReduce then do
          markReduction
          return (Lam lam_lab x b')
        else
          return term'
      App app_lab f a -> do
        f' <- step f
        didReduce <- hasReduced
        if didReduce then do
          markReduction
          return (App app_lab f' a)
        else do
          resetReduction
          a' <- step a
          didReduce <- hasReduced
          if didReduce then do
            markReduction
            return (App app_lab f a')
          else
            return term'
      Sup sup_lab a b -> do
        a' <- step a
        didReduce <- hasReduced
        if didReduce then do
          markReduction
          return (Sup sup_lab a' b)
        else do
          resetReduction
          b' <- step b
          didReduce <- hasReduced
          if didReduce then do
            markReduction
            return (Sup sup_lab a b')
          else
            return term'
      Dup dup_lab r s v k -> do
        v' <- step v
        didReduce <- hasReduced
        if didReduce then do
          markReduction
          return (Dup dup_lab r s v' k)
        else do
          resetReduction
          k' <- step k
          didReduce <- hasReduced
          if didReduce then do
            markReduction
            return (Dup dup_lab r s v k')
          else
            return term'
      _ -> return term'

normal :: Term -> IO Term
normal term = do
  substStr <- showSubst
  putStrLn $ substStr ++ show term
  putStrLn $ replicate 40 '-'
  term' <- step term
  did_reduce <- hasReduced
  if did_reduce then
    normal term'
  else
    return term'

-- Stringifier
-- -----------

name :: Name -> String
name k = all !! fromIntegral (k+1) where
  all :: [String]
  all = [""] ++ concatMap (\str -> map (: str) ['a'..'z']) all

instance Show Term where
  show (Var n)           = name n
  show (Let x t1 t2)     = "! " ++ name x ++ " = " ++ show t1 ++ "; " ++ show t2
  show Era               = "*"
  show (Sup l t1 t2)     
    | l == 0             = "{" ++ show t1 ++ "," ++ show t2 ++ "}"
    | l == 1             = "<" ++ show t1 ++ "," ++ show t2 ++ ">"
    | otherwise          = "&" ++ show (fromIntegral l :: Int) ++ "{" ++ show t1 ++ "," ++ show t2 ++ "}"
  show (Dup l x y t1 t2) 
    | l == 0             = "! {" ++ name x ++ "," ++ name y ++ "} = " ++ show t1 ++ "; " ++ show t2
    | l == 1             = "! <" ++ name x ++ "," ++ name y ++ "> = " ++ show t1 ++ "; " ++ show t2
    | otherwise          = "! &" ++ show (fromIntegral l :: Int) ++ "{" ++ name x ++ "," ++ name y ++ "} = " ++ show t1 ++ "; " ++ show t2
  show (Lam lab x t)     
    | lab == 0           = "λ" ++ name x ++ "." ++ show t
    | lab == 1           = "Λ" ++ name x ++ "." ++ show t
    | otherwise          = "&" ++ show (fromIntegral lab :: Int) ++ " λ" ++ name x ++ "." ++ show t
  show (App lab t1 t2)   
    | lab == 0           = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    | lab == 1           = "[" ++ show t1 ++ " " ++ show t2 ++ "]"
    | otherwise          = "&" ++ show (fromIntegral lab :: Int) ++ " (" ++ show t1 ++ " " ++ show t2 ++ ")"

-- Parser
-- ------

type ParserST = Map.Map String Name
type LocalCtx = Map.Map String Name
type Parser a = ParsecT String ParserST IO a

whiteSpace :: Parser ()
whiteSpace = skipMany (space <|> comment) where
  comment = do 
    try (string "//")
    skipMany (noneOf "\n\r")
    (newline <|> (eof >> return '\n'))

lexeme :: Parser a -> Parser a
lexeme p = p <* whiteSpace

symbol :: String -> Parser String
symbol s = lexeme (string s)

parseNatural :: Parser Integer
parseNatural = lexeme $ read <$> many1 digit

isGlobal :: String -> Bool
isGlobal name = take 1 name == "$"

getGlobalName :: String -> Parser Name
getGlobalName gname = do
  globalMap <- getState
  case Map.lookup gname globalMap of
    Just n  -> return n
    Nothing -> do
      n <- liftIO fresh
      putState (Map.insert gname n globalMap)
      return n

bindVar :: String -> LocalCtx -> Parser (Name, LocalCtx)
bindVar name ctx
  | isGlobal name = do
      n <- getGlobalName name
      return (n, ctx)
  | otherwise = do
      n <- liftIO fresh
      let ctx' = Map.insert name n ctx
      return (n, ctx')

getVar :: String -> LocalCtx -> Parser Name
getVar name ctx
  | isGlobal name = getGlobalName name
  | otherwise = case Map.lookup name ctx of
      Just n  -> return n
      Nothing -> fail $ "Unbound local variable: " ++ name

parseVarName :: Parser String
parseVarName = lexeme $ try (do
  char '$'
  name <- many1 (alphaNum <|> char '_')
  return ("$" ++ name)
 ) <|> many1 (alphaNum <|> char '_')

-- Term parsers
parseTerm :: LocalCtx -> Parser Term
parseTerm ctx
  =   try (parseApp ctx)
  <|> try (parseLet ctx)
  <|> try (parseLam ctx)
  <|> try (parseSup ctx)
  <|> try (parseDup ctx)
  <|> parseSimpleTerm ctx

parseSimpleTerm :: LocalCtx -> Parser Term
parseSimpleTerm ctx
  = parseVar ctx
  <|> parseEra
  <|> between (symbol "(") (symbol ")") (parseTerm ctx)

parseVar :: LocalCtx -> Parser Term
parseVar ctx = do
  name <- parseVarName
  n    <- getVar name ctx
  return $ Var n

parseLam :: LocalCtx -> Parser Term
parseLam ctx = try (parseLamWithLabel ctx) <|> parseSimpleLam ctx <|> parseCapitalLam ctx

parseSimpleLam :: LocalCtx -> Parser Term
parseSimpleLam ctx = do
  symbol "λ"
  name      <- parseVarName
  (n, ctx') <- bindVar name ctx
  symbol "."
  body      <- parseTerm ctx'
  return $ Lam 0 n body

parseCapitalLam :: LocalCtx -> Parser Term
parseCapitalLam ctx = do
  symbol "Λ"
  name      <- parseVarName
  (n, ctx') <- bindVar name ctx
  symbol "."
  body      <- parseTerm ctx'
  return $ Lam 1 n body

parseLamWithLabel :: LocalCtx -> Parser Term
parseLamWithLabel ctx = do
  symbol "&"
  lab <- fromIntegral <$> parseNatural
  symbol "λ"
  name      <- parseVarName
  (n, ctx') <- bindVar name ctx
  symbol "."
  body      <- parseTerm ctx'
  return $ Lam lab n body

parseApp :: LocalCtx -> Parser Term
parseApp ctx = try (parseAppWithLabel ctx) <|> parseSimpleApp ctx <|> parseSquareApp ctx

parseSimpleApp :: LocalCtx -> Parser Term
parseSimpleApp ctx = between (symbol "(") (symbol ")") $ do
  f <- parseTerm ctx
  whiteSpace
  a <- parseTerm ctx
  return $ App 0 f a

parseSquareApp :: LocalCtx -> Parser Term
parseSquareApp ctx = between (symbol "[") (symbol "]") $ do
  f <- parseTerm ctx
  whiteSpace
  a <- parseTerm ctx
  return $ App 1 f a

parseAppWithLabel :: LocalCtx -> Parser Term
parseAppWithLabel ctx = do
  symbol "&"
  lab <- fromIntegral <$> parseNatural
  between (symbol "(") (symbol ")") $ do
    f <- parseTerm ctx
    whiteSpace
    a <- parseTerm ctx
    return $ App lab f a

parseSup :: LocalCtx -> Parser Term
parseSup ctx = try (parseSupWithLabel ctx) <|> parseSimpleSup ctx <|> parseAngleSup ctx

parseSimpleSup :: LocalCtx -> Parser Term
parseSimpleSup ctx = between (symbol "{") (symbol "}") $ do
  a <- parseTerm ctx
  symbol ","
  b <- parseTerm ctx
  return $ Sup 0 a b

parseAngleSup :: LocalCtx -> Parser Term
parseAngleSup ctx = between (symbol "<") (symbol ">") $ do
  a <- parseTerm ctx
  symbol ","
  b <- parseTerm ctx
  return $ Sup 1 a b

parseSupWithLabel :: LocalCtx -> Parser Term
parseSupWithLabel ctx = do
  symbol "&"
  l <- fromIntegral <$> parseNatural
  between (symbol "{") (symbol "}") $ do
    a <- parseTerm ctx
    symbol ","
    b <- parseTerm ctx
    return $ Sup l a b

parseDup :: LocalCtx -> Parser Term
parseDup ctx = try (parseDupWithLabel ctx) <|> parseSimpleDup ctx <|> parseAngleDup ctx

parseSimpleDup :: LocalCtx -> Parser Term
parseSimpleDup ctx = do
  symbol "!"
  (name1, name2) <- between (symbol "{") (symbol "}") $ do
    a <- parseVarName
    symbol ","
    b <- parseVarName
    return (a, b)
  symbol "="
  val <- parseTerm ctx
  symbol ";"
  (n1, ctx') <- bindVar name1 ctx
  (n2, ctx'') <- bindVar name2 ctx'
  body <- parseTerm ctx''
  return $ Dup 0 n1 n2 val body

parseAngleDup :: LocalCtx -> Parser Term
parseAngleDup ctx = do
  symbol "!"
  (name1, name2) <- between (symbol "<") (symbol ">") $ do
    a <- parseVarName
    symbol ","
    b <- parseVarName
    return (a, b)
  symbol "="
  val <- parseTerm ctx
  symbol ";"
  (n1, ctx') <- bindVar name1 ctx
  (n2, ctx'') <- bindVar name2 ctx'
  body <- parseTerm ctx''
  return $ Dup 1 n1 n2 val body

parseDupWithLabel :: LocalCtx -> Parser Term
parseDupWithLabel ctx = do
  symbol "!"
  symbol "&"
  l <- fromIntegral <$> parseNatural
  (name1, name2) <- between (symbol "{") (symbol "}") $ do
    a <- parseVarName
    symbol ","
    b <- parseVarName
    return (a, b)
  symbol "="
  val <- parseTerm ctx
  symbol ";"
  (n1, ctx') <- bindVar name1 ctx
  (n2, ctx'') <- bindVar name2 ctx'
  body <- parseTerm ctx''
  return $ Dup l n1 n2 val body

parseLet :: LocalCtx -> Parser Term
parseLet ctx = do
  symbol "!"
  name      <- parseVarName
  symbol "="
  t1        <- parseTerm ctx
  symbol ";"
  (n, ctx') <- bindVar name ctx
  t2        <- parseTerm ctx'
  return $ Let n t1 t2

parseEra :: Parser Term
parseEra = do
  symbol "*"
  return Era

parseIC :: String -> IO (Either ParseError (Term, Map.Map String Name))
parseIC input = runParserT parser Map.empty "" input where
  parser = do
    whiteSpace
    term <- parseTerm Map.empty
    state <- getState
    return (term, state)

doParseIC :: String -> IO Term
doParseIC input = do
  result <- parseIC input
  case result of
    Left err        -> error $ show err
    Right (term, _) -> return term

-- Tests
-- -----

test_term :: String -> IO ()
test_term input = do
  term <- doParseIC input
  _ <- normal term
  inters <- readIORef gINTERS
  putStrLn $ "- WORK: " ++ show inters

test_ic :: IO ()
test_ic = do

  test_term $ """
!F = λf.
  !{f0,f1} = f;
  !{f0,f1} = λx.(f0 (f1 x));
  λx.(f0 (f1 x));
((F λnx.((nx λt0.λf0.f0) λt1.λf1.t1)) λT.λF.T)
"""
  inters <- readIORef gINTERS
  putStrLn $ "- WORK: " ++ show inters

main :: IO ()
main = test_ic
