{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Monad (replicateM)
import Data.Bits (shiftL)
import Data.Char (chr, ord)
import Data.IORef
import Data.List (foldl')
import System.CPUTime
import Text.ParserCombinators.ReadP
import Text.Printf
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

-- Types
-- =====

type Lab  = Int
type Name = Int

data Term
  = Var !Name
  | Dp0 !Name
  | Dp1 !Name
  | Era
  | Sup !Lab !Term !Term
  | Dup !Name !Lab !Term !Term
  | Lam !Name !Term
  | App !Term !Term

-- Name Encoding/Decoding
-- ======================

-- Base-64 encoding (for parsing user names/labels and printing)
-- Alphabet: _ (0), a-z (1-26), A-Z (27-52), 0-9 (53-62), $ (63).
alphabet :: String
alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"

char_map :: M.Map Char Int
char_map = M.fromList (zip alphabet [0..])

name_to_int :: String -> Int
name_to_int = foldl' go 0
  where go acc c = (acc `shiftL` 6) + char_map M.! c

int_to_name :: Int -> String
int_to_name 0 = "_"
int_to_name n = reverse $ go n
  where go 0 = ""
        go m = let (q,r) = m `divMod` 64
               in alphabet !! r : go q

-- Parsing
-- =======

lexeme :: ReadP a -> ReadP a
lexeme p = skipSpaces *> p

parse_nam :: ReadP String
parse_nam = lexeme $ munch1 (`M.member` char_map)

parse_term :: ReadP Term
parse_term = lexeme $ choice
  [ parse_lam, parse_dup, parse_app, parse_sup, parse_era, parse_var ]

parse_app :: ReadP Term
parse_app = between (lexeme (char '(')) (lexeme (char ')')) $ do
  ts <- many1 parse_term
  let (t:rest) = ts
  return (foldl' App t rest)

parse_lam :: ReadP Term
parse_lam = do
  lexeme (choice [char 'λ', char '\\'])
  k <- parse_nam
  lexeme (char '.')
  body <- parse_term
  return (Lam (name_to_int k) body)

parse_dup :: ReadP Term
parse_dup = do
  lexeme (char '!')
  k <- parse_nam
  lexeme (char '&')
  l <- parse_nam
  lexeme (char '=')
  v <- parse_term
  lexeme (char ';')
  t <- parse_term
  return (Dup (name_to_int k) (name_to_int l) v t)

parse_sup :: ReadP Term
parse_sup = do
  lexeme (char '&')
  l <- parse_nam
  between (lexeme (char '{')) (lexeme (char '}')) $ do
    a <- parse_term
    lexeme (char ',')
    b <- parse_term
    return (Sup (name_to_int l) a b)

parse_era :: ReadP Term
parse_era = lexeme (string "&{}") >> return Era

parse_var :: ReadP Term
parse_var = do
  k <- parse_nam
  let kid = name_to_int k
  choice
    [ string "₀"  >> return (Dp0 kid)
    , string "₁"  >> return (Dp1 kid)
    , return (Var kid)
    ]

read_term :: String -> Term
read_term s = case readP_to_S (parse_term <* skipSpaces <* eof) s of
  [(t, "")] -> t
  _         -> error "bad-parse"

-- Environment
-- ===========

data Env = Env
  { inters  :: !(IORef Int)
  , id_new  :: !(IORef Int)
  , var_map :: !(IORef (IM.IntMap Term))
  , dp0_map :: !(IORef (IM.IntMap Term))
  , dp1_map :: !(IORef (IM.IntMap Term))
  , dup_map :: !(IORef (IM.IntMap (Lab, Term)))
  }

new_env :: IO Env
new_env = do
  itr <- newIORef 0
  ids <- newIORef 0
  vm  <- newIORef IM.empty
  d0m <- newIORef IM.empty
  d1m <- newIORef IM.empty
  dm  <- newIORef IM.empty
  return $ Env itr ids vm d0m d1m dm

inc_inters :: Env -> IO ()
inc_inters e = do
  !n <- readIORef (inters e)
  writeIORef (inters e) (n + 1)

fresh :: Env -> IO Name
fresh e = do
  !n <- readIORef (id_new e)
  writeIORef (id_new e) (n + 1)
  return ((n `shiftL` 6) + 63)

subst_var :: Env -> Name -> Term -> IO ()
subst_var e k v = modifyIORef' (var_map e) (IM.insert k v)

subst_dp0 :: Env -> Name -> Term -> IO ()
subst_dp0 e k v = modifyIORef' (dp0_map e) (IM.insert k v)

subst_dp1 :: Env -> Name -> Term -> IO ()
subst_dp1 e k v = modifyIORef' (dp1_map e) (IM.insert k v)

delay_dup :: Env -> Name -> Lab -> Term -> IO ()
delay_dup e k l v = modifyIORef' (dup_map e) (IM.insert k (l, v))

taker :: IORef (IM.IntMap a) -> Name -> IO (Maybe a)
taker ref k = do
  !m <- readIORef ref
  case IM.lookup k m of
    Nothing -> return Nothing
    Just v  -> do
      writeIORef ref (IM.delete k m)
      return (Just v)

take_var :: Env -> Name -> IO (Maybe Term)
take_var e = taker (var_map e)

take_dp0 :: Env -> Name -> IO (Maybe Term)
take_dp0 e = taker (dp0_map e)

take_dp1 :: Env -> Name -> IO (Maybe Term)
take_dp1 e = taker (dp1_map e)

take_dup :: Env -> Name -> IO (Maybe (Lab, Term))
take_dup e = taker (dup_map e)

-- Evaluation (Weak Head Normal Form)
-- ==================================

wnf :: Env -> Term -> IO Term
wnf e (App f x) = do
  !f0 <- wnf e f
  app e f0 x
wnf e (Dup k l v t) = do
  delay_dup e k l v
  wnf e t
wnf e (Var x) = var e x
wnf e (Dp0 x) = dp0 e x
wnf e (Dp1 x) = dp1 e x
wnf e f = return f

app :: Env -> Term -> Term -> IO Term
app e (Lam fk ff)    x = app_lam e fk ff x
app e (Sup fl fa fb) x = app_sup e fl fa fb x
app e f              x = return $ App f x

dup :: Env -> Name -> Lab -> Term -> Term -> IO Term
dup e k l (Lam vk vf)    t = dup_lam e k l vk vf t
dup e k l (Sup vl va vb) t = dup_sup e k l vl va vb t
dup e k l v              t = return $ Dup k l v t

-- (λx.f a)
-- -------- app-lam
-- x ← a
-- f

app_lam :: Env -> Name -> Term -> Term -> IO Term
app_lam e fx ff v = do
  inc_inters e
  subst_var e fx v
  wnf e ff

-- (&L{f,g} a)
-- ----------------- app-sup
-- ! A &L = a
-- &L{(f A₀),(g A₁)}

app_sup :: Env -> Lab -> Term -> Term -> Term -> IO Term
app_sup e fL fa fb v = do
  inc_inters e
  x <- fresh e
  delay_dup e x fL v
  wnf e (Sup fL (App fa (Dp0 x)) (App fb (Dp1 x)))


-- ! F &L = λx.f
-- ---------------- dup-lam
-- F₀ ← λ$x0.G₀
-- F₁ ← λ$x1.G₁
-- x  ← &L{$x0,$x1}
-- ! G &L = f

dup_lam :: Env -> Name -> Lab -> Name -> Term -> Term -> IO Term
dup_lam e k l vk vf t = do
  inc_inters e
  x0 <- fresh e
  x1 <- fresh e
  g  <- fresh e
  subst_dp0 e k (Lam x0 (Dp0 g))
  subst_dp1 e k (Lam x1 (Dp1 g))
  subst_var e vk (Sup l (Var x0) (Var x1))
  delay_dup e g l vf
  wnf e t

-- ! X &L = &R{a,b}
-- ---------------- dup-sup
-- if L == R:
--   X₀ ← a
--   X₁ ← b
-- else:
--   ! A &L = a
--   ! B &L = b
--   X₀ ← &R{A₀,B₀}
--   X₁ ← &R{A₁,B₁}

dup_sup :: Env -> Name -> Lab -> Lab -> Term -> Term -> Term -> IO Term
dup_sup e k l vl va vb t
  | l == vl = do
      inc_inters e
      subst_dp0 e k va
      subst_dp1 e k vb
      wnf e t
  | otherwise = do
      inc_inters e
      a <- fresh e
      b <- fresh e
      subst_dp0 e k (Sup vl (Dp0 a) (Dp0 b))
      subst_dp1 e k (Sup vl (Dp1 a) (Dp1 b))
      delay_dup e a l va
      delay_dup e b l vb
      wnf e t

var :: Env -> Name -> IO Term
var e k = do
  mt <- take_var e k
  case mt of
    Just t  -> wnf e t
    Nothing -> return $ Var k

dpx :: (Env -> Name -> IO (Maybe Term)) -> (Name -> Term) -> Env -> Name -> IO Term
dpx take_dp taker e k = do
  mt <- take_dp e k
  case mt of
    Just t  -> wnf e t
    Nothing -> do
      mlv <- take_dup e k
      case mlv of
        Just (l, v) -> do
          !v0 <- wnf e v
          dup e k l v0 (taker k)
        Nothing -> return $ taker k

dp0 :: Env -> Name -> IO Term
dp0 = dpx take_dp0 Dp0

dp1 :: Env -> Name -> IO Term
dp1 = dpx take_dp1 Dp1

-- Normalization
-- =============

nf :: Env -> Int -> Term -> IO Term
nf e d x = do { !x0 <- wnf e x ; go e d x0 } where
  go :: Env -> Int -> Term -> IO Term
  go e d (Var k) = do
    return $ Var k
  go e d (Dp0 k) = do
    return $ Dp0 k
  go e d (Dp1 k) = do
    return $ Dp1 k
  go e d Era = do
    return Era
  go e d (App f x) = do
    !f0 <- nf e d f
    !x0 <- nf e d x
    return $ App f0 x0
  go e d (Sup l a b) = do
    !a0 <- nf e d a
    !b0 <- nf e d b
    return $ Sup l a0 b0
  go e d (Lam k f) = do
    subst_var e k (Var d)
    !f0 <- nf e (d + 1) f
    return $ Lam d f0
  go e d (Dup k l v t) = do
    !v0 <- nf e d v
    subst_dp0 e k (Dp0 d)
    subst_dp1 e k (Dp1 d)
    !t0 <- nf e (d + 1) t
    return $ Dup d l v0 t0

-- Showing
-- =======

instance Show Term where
  show (Var k)       = int_to_name (k+1)
  show (Dp0 k)       = int_to_name (k+1) ++ "₀"
  show (Dp1 k)       = int_to_name (k+1) ++ "₁"
  show Era           = "&{}"
  show (Sup l a b)   = "&" ++ int_to_name l ++ "{" ++ show a ++ "," ++ show b ++ "}"
  show (Dup k l v t) = "!" ++ int_to_name (k+1) ++ "&" ++ int_to_name l ++ "=" ++ show v ++ ";" ++ show t
  show (Lam k f)     = "λ" ++ int_to_name (k+1) ++ "." ++ show f
  show (App f x)     = "(" ++ show f ++ " " ++ show x ++ ")"

-- Benchmark term generator
-- =========================

-- Generates the church-encoded exponentiation benchmark term.
f :: Int -> String
f n = "λf. " ++ dups ++ final where
  dups  = concat [dup i | i <- [0..n-1]]
  dup 0 = "!F00 &A = f;\n    "
  dup i = "!F" ++ pad i ++ " &A = λx" ++ pad (i-1) ++ ".(F" ++ pad (i-1) ++ "₀ (F" ++ pad (i-1) ++ "₁ x" ++ pad (i-1) ++ "));\n    "
  final = "λx" ++ pad (n-1) ++ ".(F" ++ pad (n-1) ++ "₀ (F" ++ pad (n-1) ++ "₁ x" ++ pad (n-1) ++ "))"
  pad x = if x < 10 then "0" ++ show x else show x

-- Main
-- ====

main :: IO ()
main = do
  -- Benchmark configuration: 2^22
  let n = 20
  -- The term applies (2^22) to the 'False' church numeral (λT.λF.F), resulting in 'True' (λT.λF.T).
  let term_str = "((" ++ f n ++ " λX.((X λT0.λF0.F0) λT1.λF1.T1)) λT2.λF2.T2)"

  -- Parse directly to Term.
  let term = read_term term_str

  -- Setup environment (fresh IDs start automatically at 0 and get '$' prepended).
  env <- new_env

  -- Execution
  start <- getCPUTime
  !res <- nf env 0 term -- Start normalization with depth 0
  end <- getCPUTime

  -- Output
  interactions <- readIORef (inters env)
  let diff = fromIntegral (end - start) / (10^12)
  let rate = fromIntegral interactions / diff

  -- Expected output: λa.λb.a (Canonical representation of Church True)
  putStrLn (show res)
  print interactions
  printf "Time: %.3f seconds\n" (diff :: Double)
  printf "Rate: %.2f M interactions/s\n" (rate / 1000000 :: Double)
