-- Calculus of Interactions
-- ========================
-- CoI is a term rewrite system for the following grammar:
-- 
-- Term ::=
-- | Var ::= Name
-- | Dp0 ::= Name "₀"
-- | Dp1 ::= Name "₁"
-- | Era ::= "&{}"
-- | Sup ::= "&" Name "{" Term "," Term "}"
-- | Dup ::= "!" Name "&" Name "=" Term ";" Term
-- | Lam ::= "λ" Name "." Term
-- | App ::= "(" Term " " Term ")"
-- | Zer ::= "0"
-- | Suc ::= "1+"
-- | Ref ::= "@" Name
-- | Cal ::= Term "~>" Term
--
-- Where:
-- - Name ::= any sequence of base-64 chars in _ A-Z a-z 0-9 $
-- - [T]  ::= any sequence of T separated by ","
-- 
-- In CoI:
-- - Variables are affine (they must occur at most once)
-- - Variables range globally (they can occur anywhere)
-- 
-- Terms are rewritten via the following interaction rules:
-- 
-- (λx.f a)
-- -------- app-lam
-- x ← a
-- f
-- 
-- (&L{f,g} a)
-- ----------------- app-sup
-- ! A &L = a
-- &L{(f A₀),(g A₁)}
-- 
-- ! X &L = &{}
-- ------------ dup-era
-- X₀ ← &{}
-- X₁ ← &{}
-- 
-- ! F &L = λx.f
-- ---------------- dup-lam
-- F₀ ← λ$x0.G₀
-- F₁ ← λ$x1.G₁
-- x  ← &L{$x0,$x1}
-- ! G &L = f
-- 
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
--
-- ! X &L = 0
-- ---------- dup-zer
-- X₀ ← 0
-- X₁ ← 0
-- 
-- ! X &L = 1+n
-- ------------ dup-suc
-- ! N &L = n
-- X₀ ← 1+N₀
-- X₁ ← 1+N₁
--
-- ! X &L = Λ{0:z;1+:s}
-- -------------------- dup-swi
-- ! Z &L = z
-- ! S &L = s
-- X₀ ← Λ{0:Z₀;1+:S₀}
-- X₁ ← Λ{0:Z₁;1+:S₁}
-- 
-- @foo
-- ------------------ ref
-- foo ~> Book["foo"]
-- 
-- ((f ~> λx.g) a)
-- --------------- app-cal-lam
-- x ← a
-- (f x) ~> g
-- 
-- ((f ~> Λ{0:z;1+:s}) 0)
-- ---------------------- app-cal-swi-zer
-- (f 0) ~> z
-- 
-- ((f ~> Λ{0:z;1+:s}) 1+n)
-- ------------------------ app-cal-swi-suc
-- ((λp.(f 1+p) ~> s) n)
-- 
-- ((f ~> Λ{0:z;1+:s}) &L{a,b})
-- ---------------------------- app-cal-swi-sup
-- ! &L F = f
-- ! &L Z = z
-- ! &L N = s
-- &L{((F₀ ~> Λ{0:Z₀;1+:N₀}) a)
--   ,((F₁ ~> Λ{0:Z₁;1+:N₁}) b)}
-- 
-- ((f ~> &L{x,y}) a)
-- ------------------ app-cal-sup
-- ! F &L = f
-- ! A &L = a
-- &L{((F₀ ~> x) A₀))
--   ,((F₁ ~> y) A₁))}
-- 
-- ! &L X = f ~> g
-- --------------- dup-cal
-- ! F &L = f
-- ! G &L = g
-- X₀ ← F₀ ~> G₀ 
-- X₁ ← F₁ ~> G₁

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Monad (when, forM_)
import Data.Bits (shiftL)
import Data.Char (isDigit)
import Data.IORef
import Data.List (foldl', elemIndex)
import System.CPUTime
import Text.ParserCombinators.ReadP
import Text.Printf
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

debug :: Bool
debug = False

-- Types
-- =====

type Lab  = Int
type Name = Int

data Term
  = Nam !String
  | Dry !Term !Term
  | Var !Name
  | Dp0 !Name
  | Dp1 !Name
  | Era
  | Sup !Lab !Term !Term
  | Dup !Name !Lab !Term !Term
  | Lam !Name !Term
  | App !Term !Term
  | Zer
  | Suc !Term
  | Swi !Term !Term
  | Ref !Name
  | Cal !Term !Term
  deriving (Eq)

data Kind
  = VAR
  | DP0
  | DP1
  deriving (Enum)

data Book = Book (M.Map Name Term)

data Env = Env
  { env_book    :: !Book
  , env_inters  :: !(IORef Int)
  , env_new_id  :: !(IORef Int)
  , env_sub_map :: !(IORef (IM.IntMap Term))
  , env_dup_map :: !(IORef (IM.IntMap (Lab, Term)))
  }

-- Showing
-- =======

instance Show Term where
  show (Nam k)       = (if debug then "." else "") ++ k
  show (Dry f x)     = (if debug then "." else "") ++ show_app f [x]
  show (Var k)       = int_to_name k
  show (Dp0 k)       = int_to_name k ++ "₀"
  show (Dp1 k)       = int_to_name k ++ "₁"
  show Era           = "&{}"
  show (Sup l a b)   = "&" ++ int_to_name l ++ "{" ++ show a ++ "," ++ show b ++ "}"
  show (Dup k l v t) = "!" ++ int_to_name k ++ "&" ++ int_to_name l ++ "=" ++ show v ++ ";" ++ show t
  show (Lam k f)     = "λ" ++ int_to_name k ++ "." ++ show f
  show (App f x)     = show_app f [x]
  show Zer           = "0"
  show (Suc p)       = show_add 1 p
  show (Swi z s)     = "λ{0:" ++ show z ++ ";1+:" ++ show s ++ "}"
  show (Ref k)       = "@" ++ int_to_name k
  show (Cal f g)     = show f ++ "~>" ++ show g

show_add :: Int -> Term -> String
show_add n (Suc p) = show_add (n + 1) p
show_add n Zer     = show n
show_add n term    = show n ++ "+" ++ show term

show_app :: Term -> [Term] -> String
show_app (Dry f x) args = show_app f (x : args)
show_app (App f x) args = show_app f (x : args)
show_app f         args = "(" ++ unwords (map show (f : args)) ++ ")"

instance Show Book where
  show (Book m) = unlines [ "@" ++ int_to_name k ++ " = " ++ show ct | (k, ct) <- M.toList m ]

show_dup_map :: IM.IntMap (Lab, Term) -> String
show_dup_map m = unlines [ "! " ++ int_to_name k ++ " &" ++ int_to_name l ++ " = " ++ show v | (k, (l, v)) <- IM.toList m ]

show_sub_map :: IM.IntMap Term -> String
show_sub_map m = unlines [ int_to_name (k `div` 4) ++ suffix (k `mod` 4) ++ " ← " ++ show v | (k, v) <- IM.toList m ]
  where suffix x = case x of { 0 -> "" ; 1 -> "₀" ; 2 -> "₁" ; _ -> "?" }

-- Name Encoding/Decoding
-- ======================

-- Base-64 encoding (for parsing user names/labels and printing)
-- Alphabet: _ (0), a-z (1-26), A-Z (27-52), 0-9 (53-62), $ (63).
alphabet :: String
alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"

alphabet_first :: String
alphabet_first = filter (`notElem` "_0123456789") alphabet

name_to_int :: String -> Int
name_to_int = foldl' (\acc c -> (acc `shiftL` 6) + idx c) 0
  where idx c = maybe (error "bad name char") id (elemIndex c alphabet)

int_to_name :: Int -> String
int_to_name 0 = "_"
int_to_name n = reverse (go n)
  where go 0 = ""
        go m = let (q,r) = m `divMod` 64 in alphabet !! r : go q

-- Parsing
-- =======

lexeme :: ReadP a -> ReadP a
lexeme p = skipSpaces *> p

parse_nam :: ReadP String
parse_nam = lexeme $ do
  head <- satisfy (`elem` alphabet_first)
  tail <- munch (`elem` alphabet)
  return (head : tail)

parse_term :: ReadP Term
parse_term = parse_term_base

parse_term_base :: ReadP Term
parse_term_base = lexeme $ choice
  [ parse_lam_or_swi
  , parse_dup
  , parse_app
  , parse_sup
  , parse_era
  , parse_add
  , parse_nat
  , parse_ref
  , parse_var
  ]

parse_app :: ReadP Term
parse_app = between (lexeme (char '(')) (lexeme (char ')')) $ do
  ts <- many1 parse_term
  let (t:rest) = ts
  return (foldl' App t rest)

parse_lam_or_swi :: ReadP Term
parse_lam_or_swi = do
  lexeme (choice [char 'λ', char '\\'])
  parse_swi_body <++ parse_lam_body

parse_lam_body :: ReadP Term
parse_lam_body = do
  k <- parse_nam
  lexeme (char '.')
  body <- parse_term
  return (Lam (name_to_int k) body)

parse_swi_body :: ReadP Term
parse_swi_body = do
  between (lexeme (char '{')) (lexeme (char '}')) $ do
    lexeme (string "0:")
    z <- parse_term
    optional (lexeme (char ';'))
    lexeme (string "1+:")
    s <- parse_term
    return (Swi z s)

parse_dup :: ReadP Term
parse_dup = do
  lexeme (char '!')
  k <- parse_nam
  lexeme (char '&')
  l <- parse_nam
  lexeme (char '=')
  v <- parse_term
  optional (lexeme (char ';'))
  t <- parse_term
  return (Dup (name_to_int k) (name_to_int l) v t)

parse_sup :: ReadP Term
parse_sup = do
  lexeme (char '&')
  l <- parse_nam
  between (lexeme (char '{')) (lexeme (char '}')) $ do
    a <- parse_term
    optional (lexeme (char ','))
    b <- parse_term
    return (Sup (name_to_int l) a b)

parse_era :: ReadP Term
parse_era = lexeme (string "&{}") >> return Era

parse_ref :: ReadP Term
parse_ref = do
  lexeme (char '@')
  k <- parse_nam
  return (Ref (name_to_int k))

parse_add :: ReadP Term
parse_add = do
  value <- parse_number
  skipSpaces
  _ <- char '+'
  term <- parse_term_base
  return (iterate Suc term !! value)

parse_nat :: ReadP Term
parse_nat = do
  value <- parse_number
  return (iterate Suc Zer !! value)

parse_number :: ReadP Int
parse_number = read <$> munch1 isDigit

parse_var :: ReadP Term
parse_var = do
  k <- parse_nam
  let kid = name_to_int k
  choice
    [ string "₀" >> return (Dp0 kid)
    , string "₁" >> return (Dp1 kid)
    , return (Var kid)
    ]

parse_func :: ReadP (Name, Term)
parse_func = do
  lexeme (char '@')
  k <- parse_nam
  lexeme (char '=')
  f <- parse_term
  return (name_to_int k, f)

parse_book :: ReadP Book
parse_book = do
  skipSpaces
  funcs <- many parse_func
  skipSpaces
  eof
  return $ Book (M.fromList funcs)

read_term :: String -> Term
read_term s = case readP_to_S (parse_term <* skipSpaces <* eof) s of
  [(t, "")] -> t
  _         -> error "bad-parse"

read_book :: String -> Book
read_book s = case readP_to_S parse_book s of
  [(b, "")] -> b
  _         -> error "bad-parse"

-- Environment
-- ===========

new_env :: Book -> IO Env
new_env bk = do
  itr <- newIORef 0
  ids <- newIORef 1
  sub <- newIORef IM.empty
  dm  <- newIORef IM.empty
  return $ Env bk itr ids sub dm

inc_inters :: Env -> IO ()
inc_inters e = do
  !n <- readIORef (env_inters e)
  writeIORef (env_inters e) (n + 1)

fresh :: Env -> IO Name
fresh e = do
  !n <- readIORef (env_new_id e)
  writeIORef (env_new_id e) (n + 1)
  return ((n `shiftL` 6) + 63)

taker :: IORef (IM.IntMap a) -> Int -> IO (Maybe a)
taker ref k = do
  !m <- readIORef ref
  case IM.lookup k m of
    Nothing -> do
      return Nothing
    Just v  -> do
      writeIORef ref (IM.delete k m)
      return (Just v)

take_dup :: Env -> Name -> IO (Maybe (Lab, Term))
take_dup e k = taker (env_dup_map e) k

take_sub :: Kind -> Env -> Name -> IO (Maybe Term)
take_sub ki e k = taker (env_sub_map e) (k `shiftL` 2 + fromEnum ki)

subst :: Kind -> Env -> Name -> Term -> IO ()
subst s e k v = modifyIORef' (env_sub_map e) (IM.insert (k `shiftL` 2 + fromEnum s) v)

duply :: Env -> Name -> Lab -> Term -> IO ()
duply e k l v = modifyIORef' (env_dup_map e) (IM.insert k (l, v))

clone :: Env -> Lab -> Term -> IO (Term, Term)
clone e l v = do
  k <- fresh e
  duply e k l v
  return $ (Dp0 k , Dp1 k)

clones :: Env -> Lab -> [Term] -> IO ([Term],[Term])
clones e l []       = return $ ([],[])
clones e l (x : xs) = do
  (x0 , x1 ) <- clone  e l x
  (xs0, xs1) <- clones e l xs
  return $ (x0 : xs0 , x1 : xs1)

-- WNF: Weak Normal Form
-- =====================

data Frame
  = FApp Term
  | FDp0 Name Lab
  | FDp1 Name Lab

type Stack = [Frame]

wnf :: Env -> Stack -> Term -> IO Term
wnf = wnf_enter

-- WNF: Enter
-- ----------

wnf_enter :: Env -> Stack -> Term -> IO Term

wnf_enter e s (App f x) = do
  when debug $ putStrLn $ ">> wnf_enter_app        : " ++ show (App f x)
  wnf_enter e (FApp x : s) f

wnf_enter e s (Var k) = do
  when debug $ putStrLn $ ">> wnf_enter_var        : " ++ show (Var k)
  wnf_sub VAR e s k

wnf_enter e s (Dup k l v t) = do
  when debug $ putStrLn $ ">> wnf_enter_dup        : " ++ show (Dup k l v t)
  duply e k l v
  wnf_enter e s t

wnf_enter e s (Dp0 k) = do
  when debug $ putStrLn $ ">> wnf_enter_dp0        : " ++ show (Dp0 k)
  mlv <- take_dup e k
  case mlv of
    Just (l, v) -> wnf_enter e (FDp0 k l : s) v
    Nothing     -> wnf_sub DP0 e s k

wnf_enter e s (Dp1 k) = do
  when debug $ putStrLn $ ">> wnf_enter_dp1        : " ++ show (Dp1 k)
  mlv <- take_dup e k
  case mlv of
    Just (l, v) -> wnf_enter e (FDp1 k l : s) v
    Nothing     -> wnf_sub DP1 e s k

wnf_enter e s (Ref k) = do
  when debug $ putStrLn $ ">> wnf_enter_ref        : " ++ show (Ref k)
  let (Book m) = env_book e
  case M.lookup k m of
    Just f  -> do
      inc_inters e
      g <- alloc e f
      when debug $ putStrLn $ ">> alloc                : " ++ show g
      wnf_enter e s (Cal (Nam ("@" ++ int_to_name k)) g)
    Nothing -> error $ "UndefinedReference: " ++ int_to_name k

wnf_enter e s (Cal f g) = do
  when debug $ putStrLn $ ">> wnf_enter_cal        : " ++ show f ++ "~>" ++ show g
  wnf_unwind e s (Cal f g)

wnf_enter e s f = do
  when debug $ putStrLn $ ">> wnf_enter            : " ++ show f
  wnf_unwind e s f

-- WNF: Unwind
-- -----------

wnf_unwind :: Env -> Stack -> Term -> IO Term
wnf_unwind e []    v = return v
wnf_unwind e (x:s) v = do
  when debug $ putStrLn $ ">> wnf_unwind           : " ++ show v
  case x of
    FApp x -> case v of
      Nam fk       -> wnf_app_nam e s fk x
      Dry ff fx    -> wnf_app_dry e s ff fx x
      Lam fk ff    -> wnf_app_lam e s fk ff x
      Sup fl fa fb -> wnf_app_sup e s fl fa fb x
      Cal f g      -> wnf_app_cal e s f g x
      f            -> wnf_unwind e s (App f x)
    FDp0 k l -> case v of
      Nam vk       -> wnf_dpn_nam e s k l vk       (Dp0 k)
      Dry vf vx    -> wnf_dpn_dry e s k l vf vx    (Dp0 k)
      Era          -> wnf_dpn_era e s k l          (Dp0 k)
      Lam vk vf    -> wnf_dpn_lam e s k l vk vf    (Dp0 k)
      Sup vl va vb -> wnf_dpn_sup e s k l vl va vb (Dp0 k)
      Cal vf vg    -> wnf_dpn_cal e s k l vf vg    (Dp0 k)
      Suc vp       -> wnf_dpn_suc e s k l vp       (Dp0 k)
      Zer          -> wnf_dpn_zer e s k l          (Dp0 k)
      Swi vz vs    -> wnf_dpn_swi e s k l vz vs    (Dp0 k)
      val          -> wnf_unwind  e s (Dup k l val (Dp0 k))
    FDp1 k l -> case v of
      Era          -> wnf_dpn_era e s k l          (Dp1 k)
      Dry vf vx    -> wnf_dpn_dry e s k l vf vx    (Dp1 k)
      Lam vk vf    -> wnf_dpn_lam e s k l vk vf    (Dp1 k)
      Sup vl va vb -> wnf_dpn_sup e s k l vl va vb (Dp1 k)
      Cal vf vg    -> wnf_dpn_cal e s k l vf vg    (Dp1 k)
      Suc vp       -> wnf_dpn_suc e s k l vp       (Dp1 k)
      Zer          -> wnf_dpn_zer e s k l          (Dp1 k)
      Swi vz vs    -> wnf_dpn_swi e s k l vz vs    (Dp1 k)
      Nam n        -> wnf_dpn_nam e s k l n        (Dp1 k)
      val          -> wnf_unwind  e s (Dup k l val (Dp1 k))

-- WNF: Interactions
-- -----------------

-- x | x₀ | x₁
wnf_sub :: Kind -> Env -> Stack -> Name -> IO Term
wnf_sub ki e s k = do
  when debug $ putStrLn $ "## wnf_sub              : " ++ int_to_name k
  mt <- take_sub ki e k
  case mt of
    Just t  -> wnf e s t
    Nothing -> wnf_unwind e s $ case ki of
      VAR -> Var k
      DP0 -> Dp0 k
      DP1 -> Dp1 k

-- .x
wnf_app_nam :: Env -> Stack -> String -> Term -> IO Term
wnf_app_nam e s fk v = do
  when debug $ putStrLn $ "## wnf_app_nam          : " ++ show (App (Nam fk) v)
  wnf e s (Dry (Nam fk) v)

-- .(f x)
wnf_app_dry :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_dry e s ff fx v = do
  when debug $ putStrLn $ "## wnf_app_dry          : " ++ show (App (Dry ff fx) v)
  wnf e s (Dry (Dry ff fx) v)

-- (λx.f a)
wnf_app_lam :: Env -> Stack -> Name -> Term -> Term -> IO Term
wnf_app_lam e s fx ff v = do
  when debug $ putStrLn $ "## wnf_app_lam          : " ++ show (App (Lam fx ff) v)
  inc_inters e
  subst VAR e fx v
  wnf e s ff

-- (&L{f,g} a)
wnf_app_sup :: Env -> Stack -> Lab -> Term -> Term -> Term -> IO Term
wnf_app_sup e s fL fa fb v = do
  when debug $ putStrLn $ "## wnf_app_sup          : " ++ show (App (Sup fL fa fb) v)
  inc_inters e
  (x0,x1) <- clone e fL v
  wnf e s (Sup fL (App fa x0) (App fb x1))

-- ! X &L = &{}
wnf_dpn_era :: Env -> Stack -> Name -> Lab -> Term -> IO Term
wnf_dpn_era e s k _ t = do
  when debug $ putStrLn $ "## wnf_dpn_era          : " ++ show (Dup k (name_to_int "_") Era t)
  inc_inters e
  subst DP0 e k Era
  subst DP1 e k Era
  wnf e s t

-- ! F &L = λx.f
wnf_dpn_lam :: Env -> Stack -> Name -> Lab -> Name -> Term -> Term -> IO Term
wnf_dpn_lam e s k l vk vf t = do
  when debug $ putStrLn $ "## wnf_dpn_lam          : " ++ show (Dup k l (Lam vk vf) t)
  inc_inters e
  x0      <- fresh e
  x1      <- fresh e
  (g0,g1) <- clone e l vf
  subst DP0 e k (Lam x0 g0)
  subst DP1 e k (Lam x1 g1)
  subst VAR e vk (Sup l (Var x0) (Var x1))
  wnf e s t

-- ! X &L = &R{a,b}
wnf_dpn_sup :: Env -> Stack -> Name -> Lab -> Lab -> Term -> Term -> Term -> IO Term
wnf_dpn_sup e s k l vl va vb t
  | l == vl = do
      when debug $ putStrLn $ "## wnf_dpn_sup_same     : " ++ show (Dup k l (Sup vl va vb) t)
      inc_inters e
      subst DP0 e k va
      subst DP1 e k vb
      wnf e s t
  | otherwise = do
      when debug $ putStrLn $ "## wnf_dpn_sup_diff     : " ++ show (Dup k l (Sup vl va vb) t)
      inc_inters e
      (a0,a1) <- clone e l va
      (b0,b1) <- clone e l vb
      subst DP0 e k (Sup vl a0 b0)
      subst DP1 e k (Sup vl a1 b1)
      wnf e s t

-- ! X &L = 0
wnf_dpn_zer :: Env -> Stack -> Name -> Lab -> Term -> IO Term
wnf_dpn_zer e s k _ t = do
  when debug $ putStrLn $ "## wnf_dpn_zer          : " ++ show (Dup k (name_to_int "_") Zer t)
  inc_inters e
  subst DP0 e k Zer
  subst DP1 e k Zer
  wnf e s t

-- ! X &L = 1+n
wnf_dpn_suc :: Env -> Stack -> Name -> Lab -> Term -> Term -> IO Term
wnf_dpn_suc e s k l p t = do
  when debug $ putStrLn $ "## wnf_dpn_suc          : " ++ show (Dup k l (Suc p) t)
  inc_inters e
  (n0,n1) <- clone e l p
  subst DP0 e k (Suc n0)
  subst DP1 e k (Suc n1)
  wnf e s t

-- ! X &L = Λ{0:z;1+:s}
wnf_dpn_swi :: Env -> Stack -> Name -> Lab -> Term -> Term -> Term -> IO Term
wnf_dpn_swi e s k l vz vs t = do
  when debug $ putStrLn $ "## wnf_dpn_swi          : " ++ show (Dup k l (Swi vz vs) t)
  inc_inters e
  (z0,z1) <- clone e l vz
  (s0,s1) <- clone e l vs
  subst DP0 e k (Swi z0 s0)
  subst DP1 e k (Swi z1 s1)
  wnf e s t

-- ! X &L = .x
wnf_dpn_nam :: Env -> Stack -> Name -> Lab -> String -> Term -> IO Term
wnf_dpn_nam e s k _ n t = do
  when debug $ putStrLn $ "## wnf_dpn_nam          : " ++ show (Dup k (name_to_int "_") (Nam n) t)
  inc_inters e
  subst DP0 e k (Nam n)
  subst DP1 e k (Nam n)
  wnf e s t

-- ! X &L = .(f x)
wnf_dpn_dry :: Env -> Stack -> Name -> Lab -> Term -> Term -> Term -> IO Term
wnf_dpn_dry e s k l vf vx t = do
  when debug $ putStrLn $ "## wnf_dpn_dry          : " ++ show (Dup k l (Dry vf vx) t)
  inc_inters e
  (f0,f1) <- clone e l vf
  (x0,x1) <- clone e l vx
  subst DP0 e k (Dry f0 x0)
  subst DP1 e k (Dry f1 x1)
  wnf e s t

-- ((f ~> g) a)
wnf_app_cal :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_cal e s f g a = do
  when debug $ putStrLn $ "## wnf_app_cal          : " ++ show f ++ "~>" ++ show g ++ " " ++ show a
  !g_wnf <- wnf e [] g
  case g_wnf of
    Lam fx ff    -> wnf_app_cal_lam e s f fx ff a
    Swi fz fs    -> wnf_app_cal_swi e s f fz fs a
    Sup fl ff fg -> wnf_app_cal_sup e s f fl ff fg a
    _            -> wnf_unwind e s (App (Cal f g_wnf) a)

-- ((f ~> λx.g) a)
wnf_app_cal_lam :: Env -> Stack -> Term -> Name -> Term -> Term -> IO Term
wnf_app_cal_lam e s f x g a = do
  when debug $ putStrLn $ "## wnf_app_cal_lam      : " ++ show (Cal f (Lam x g)) ++ " " ++ show a
  inc_inters e
  subst VAR e x a
  wnf_enter e s (Cal (App f (Var x)) g)

-- ((f ~> &L{x,y}) a)
wnf_app_cal_sup :: Env -> Stack -> Term -> Lab -> Term -> Term -> Term -> IO Term
wnf_app_cal_sup e s f l x y a = do
  when debug $ putStrLn $ "## wnf_app_cal_sup      : " ++ show (Cal f (Sup l x y)) ++ " " ++ show a
  inc_inters e
  (f0,f1) <- clone e l f
  (a0,a1) <- clone e l a
  let app0 = (App (Cal f0 x) a0)
  let app1 = (App (Cal f1 y) a1)
  wnf_enter e s (Sup l app0 app1)

-- ((f ~> Λ{0:z;1+:s}) a)
wnf_app_cal_swi :: Env -> Stack -> Term -> Term -> Term -> Term -> IO Term
wnf_app_cal_swi e s f z sc a = do
  !a_wnf <- wnf e [] a
  when debug $ putStrLn $ "## wnf_app_cal_swi      : " ++ show (Cal f (Swi z sc)) ++ " " ++ show a ++ "→" ++ show a_wnf
  case a_wnf of
    Zer       -> wnf_app_cal_swi_zer e s f z
    Suc n     -> wnf_app_cal_swi_suc e s f sc n
    Sup l b c -> wnf_app_cal_swi_sup e s f z sc l b c
    a         -> wnf_unwind e s (App f a)

-- ((f ~> Λ{0:z;1+:s}) 0)
wnf_app_cal_swi_zer :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_cal_swi_zer e s f z = do
  when debug $ putStrLn $ "## wnf_app_cal_swi_zer  : " ++ show (App (Cal f (Swi z (Nam "..."))) Zer)
  inc_inters e
  wnf_enter e s (Cal (App f Zer) z)

-- ((f ~> Λ{0:z;1+:s}) 1+n)
wnf_app_cal_swi_suc :: Env -> Stack -> Term -> Term -> Term -> IO Term
wnf_app_cal_swi_suc e s f sc n = do
  when debug $ putStrLn $ "## wnf_app_cal_swi_suc  : " ++ show (App (Cal f (Swi (Nam "...") sc)) (Suc n))
  inc_inters e
  p <- fresh e
  wnf_enter e s (App (Cal (Lam p (App f (Suc (Var p)))) sc) n)

-- ((f ~> Λ{0:z;1+:s}) &L{a,b})
wnf_app_cal_swi_sup :: Env -> Stack -> Term -> Term -> Term -> Lab -> Term -> Term -> IO Term
wnf_app_cal_swi_sup e s f z sc l a b = do
  when debug $ putStrLn $ "## wnf_app_cal_swi_sup  : " ++ show (App (Cal f (Swi z sc)) (Sup l a b))
  inc_inters e
  (f0,f1) <- clone e l f
  (z0,z1) <- clone e l z
  (s0,s1) <- clone e l sc
  let app0 = App (Cal f0 (Swi z0 s0)) a
  let app1 = App (Cal f1 (Swi z1 s1)) b
  wnf_enter e s (Sup l app0 app1)

-- ! &L X = f ~> g
wnf_dpn_cal :: Env -> Stack -> Name -> Lab -> Term -> Term -> Term -> IO Term
wnf_dpn_cal e s k l f g t = do
  when debug $ putStrLn $ "## wnf_dpn_cal          : " ++ show (Dup k l (Cal f g) t)
  inc_inters e
  (f0,f1) <- clone e l f
  (g0,g1) <- clone e l g
  subst DP0 e k (Cal f0 g0)
  subst DP1 e k (Cal f1 g1)
  wnf_enter e s t

-- Allocation
-- ==========

-- Allocates a closed term, replacing all bound names with fresh ones.
alloc :: Env -> Term -> IO Term
alloc e term = go IM.empty term where
  go :: IM.IntMap Name -> Term -> IO Term
  go m (Var k)       = return $ Var (IM.findWithDefault k k m)
  go m (Dp0 k)       = return $ Dp0 (IM.findWithDefault k k m)
  go m (Dp1 k)       = return $ Dp1 (IM.findWithDefault k k m)
  go _ Era           = return Era
  go m (Sup l a b)   = Sup l <$> go m a <*> go m b
  go m (App f x)     = App <$> go m f <*> go m x
  go _ Zer           = return Zer
  go m (Suc n)       = Suc <$> go m n
  go m (Swi z s)     = Swi <$> go m z <*> go m s
  go _ (Ref k)       = return $ Ref k
  go m (Cal f g)     = Cal <$> go m f <*> go m g
  go m (Dup k l v t) = do
    k' <- fresh e
    v' <- go m v
    t' <- go (IM.insert k k' m) t
    return $ Dup k' l v' t'
  go m (Lam k f) = do
    k' <- fresh e
    f' <- go (IM.insert k k' m) f
    return $ Lam k' f'

-- Normalization
-- =============

snf :: Env -> Int -> Term -> IO Term
snf e d x = do
  when debug $ putStrLn $ "!! nf " ++ show x
  !x' <- wnf e [] x
  when debug $ putStrLn $ "!! →→" ++ show x'
  case x' of
    Nam k -> do
      return $ Nam k
    Dry f x -> do
      f' <- snf e d f
      x' <- snf e d x
      return $ Dry f' x'
    Var k -> do
      return $ Var k
    Dp0 k -> do
      return $ Dp0 k
    Dp1 k -> do
      return $ Dp1 k
    Era -> do
      return Era
    App f x -> do
      f' <- snf e d f
      x' <- snf e d x
      return $ App f' x'
    Sup l a b -> do
      a' <- snf e d a
      b' <- snf e d b
      return $ Sup l a' b'
    Lam k f -> do
      subst VAR e k (Nam (int_to_name d))
      f' <- snf e (d + 1) f
      return $ Lam d f'
    Dup k l v t -> do
      subst DP0 e k (Nam (int_to_name d ++ "₀"))
      subst DP1 e k (Nam (int_to_name d ++ "₁"))
      v' <- snf e d v
      t' <- snf e d t
      return $ Dup d l v' t'
    Zer -> do
      return Zer
    Suc p -> do
      p' <- snf e d p
      return $ Suc p'
    Swi z s -> do
      z' <- snf e d z
      s' <- snf e d s
      return $ Swi z' s'
    Ref k -> do
      return $ Ref k
    Cal f g -> do
      g' <- snf e d g
      return g'

-- Collapsing
-- ==========

col :: Env -> Term -> IO Term
col e x = do
  !x <- wnf e [] x
  case x of
    (Sup l a b) -> do
      a' <- col e a
      b' <- col e b
      return $ Sup l a' b'
    (Suc p) -> do
      pV <- fresh e
      p' <- col e p
      inj e (Lam pV (Suc (Var pV))) [p']
    (Lam k f) -> do
      fV <- fresh e
      f' <- col e f
      inj e (Lam fV (Lam k (Var fV))) [f']
    (App f x) -> do
      fV <- fresh e
      xV <- fresh e
      f' <- col e f
      x' <- col e x
      inj e (Lam fV (Lam xV (App (Var fV) (Var xV)))) [f',x']
    (Swi z s) -> do
      zV <- fresh e
      sV <- fresh e
      z' <- col e z
      s' <- col e s
      inj e (Lam zV (Lam sV (Swi (Var zV) (Var sV)))) [z',s']
    (Cal f g) -> do 
      col e g
    x -> do
      return $ x

inj :: Env -> Term -> [Term] -> IO Term
inj e f (x : xs) = do
  !x' <- wnf e [] x
  case x' of
    (Sup l a b) -> do
      (f0  , f1 ) <- clone  e l f
      (xs0 , xs1) <- clones e l xs
      a' <- inj e f0 (a : xs0)
      b' <- inj e f1 (b : xs1)
      return $ Sup l a' b'
    x' -> do
      inj e (App f x') xs
inj e f [] = do
  return $ f

-- Main
-- ====

run :: String -> String -> IO () 
run book_src term_src = do
  let book = read_book book_src
  !env <- new_env book
  let term = read_term term_src
  !ini <- getCPUTime
  !val <- alloc env term
  !val <- col env val
  !val <- snf env 1 val
  -- !nf1 <- nf env 1 nf0
  !end <- getCPUTime
  !itr <- readIORef (env_inters env)
  let diff = fromIntegral (end - ini) / (10^12)
  let rate = fromIntegral itr / diff
  putStrLn $ show val
  putStrLn $ "- Itrs: " ++ show itr ++ " interactions"
  printf "- Time: %.3f seconds\n" (diff :: Double)
  printf "- Perf: %.2f M interactions/s\n" (rate / 1000000 :: Double)

f :: Int -> String
f n = "λf." ++ dups ++ final where
  dups  = concat [dup i | i <- [0..n-1]]
  dup 0 = "!F00&A=f;"
  dup i = "!F" ++ pad i ++ "&A=λx" ++ pad (i-1) ++ ".(F" ++ pad (i-1) ++ "₀ (F" ++ pad (i-1) ++ "₁ x" ++ pad (i-1) ++ "));"
  final = "λx" ++ pad (n-1) ++ ".(F" ++ pad (n-1) ++ "₀ (F" ++ pad (n-1) ++ "₁ x" ++ pad (n-1) ++ "))"
  pad x = if x < 10 then "0" ++ show x else show x

tests :: [(String,String)]
tests =
  [ ("(@not 0)", "1")
  , ("(@not 1+0)", "0")
  , ("!F&L=@id;!G&L=F₀;λx.(G₁ x)", "λa.a")
  , ("(@and 0 0)", "0")
  , ("(@and &L{0,1+0} 1+0)", "&L{0,1}")
  , ("(@and &L{1+0,0} 1+0)", "&L{1,0}")
  , ("(@and 1+0 &L{0,1+0})", "&L{0,1}")
  , ("(@and 1+0 &L{1+0,0})", "&L{1,0}")
  , ("λx.(@and 0 x)", "λa.(@and 0 a)")
  , ("λx.(@and x 0)", "λa.(@and a 0)")
  , ("(@sum 1+1+1+0)", "6")
  , ("λx.(@sum 1+1+1+x)", "λa.3+(@add a 2+(@add a 1+(@add a (@sum a))))")
  , ("(@foo 0)", "&L{0,0}")
  , ("(@foo 1+1+1+0)", "&L{3,2}")
  , ("λx.(@dbl 1+1+x)", "λa.4+(@dbl a)")
  , ("("++f 2++" λX.(X λT0.λF0.F0 λT1.λF1.T1) λT2.λF2.T2)", "λa.λb.a")
  , ("1+&L{0,1}", "&L{1,2}")
  , ("1+&A{&B{0,1},&C{2,3}}", "&A{&B{1,2},&C{3,4}}")
  , ("λa.!A&L=a;&L{A₀,A₁}", "&L{λa.a,λa.a}")
  , ("λa.λb.!A&L=a;!B&L=b;&L{λx.(x A₀ B₀),λx.(x A₁ B₁)}", "&L{λa.λb.λc.(c a b),λa.λb.λc.(c a b)}")
  , ("λt.(t &A{1,2} 3)", "&A{λa.(a 1 3),λa.(a 2 3)}")
  , ("λt.(t 1 &B{3,4})", "&B{λa.(a 1 3),λa.(a 1 4)}")
  , ("λt.(t &A{1,2} &A{3,4})", "&A{λa.(a 1 3),λa.(a 2 4)}")
  , ("λt.(t &A{1,2} &B{3,4})", "&A{&B{λa.(a 1 3),λa.(a 1 4)},&B{λa.(a 2 3),λa.(a 2 4)}}")
  , ("@gen", "&A{&B{λa.a,λa.1+a},&C{&D{λ{0:0;1+:λa.(@gen a)},&E{λ{0:0;1+:λa.1+(@gen a)},λ{0:0;1+:λa.2+(@gen a)}}},&D{λ{0:1;1+:λa.(@gen a)},&E{λ{0:1;1+:λa.1+(@gen a)},λ{0:1;1+:λa.2+(@gen a)}}}}}")
  , ("λx.(@gen 2+x)", "&A{&B{λa.2+a,λa.3+a},&D{λa.(@gen a),&E{λa.2+(@gen a),λa.4+(@gen a)}}}")
  , ("(@gen 2)", "&A{&B{2,3},&D{&C{0,1},&E{&C{2,3},&C{4,5}}}}")
  ]

book :: String
book = unlines
  [ "@id  = λa.a"
  , "@not = λ{0:1+0;1+:λp.0}"
  , "@dbl = λ{0:0;1+:λp.1+1+(@dbl p)}"
  , "@and = λ{0:λ{0:0;1+:λp.0};1+:λp.λ{0:0;1+:λp.1+0}}"
  , "@add = λ{0:λb.b;1+:λa.λb.1+(@add a b)}"
  , "@sum = λ{0:0;1+:λp.!P&S=p;1+(@add P₀ (@sum P₁))}"
  , "@foo = &L{λx.x,λ{0:0;1+:λp.p}}"
  , "@gen = !F&A=@gen &A{λx.!X&B=x;&B{X₀,1+X₁},λ{0:&C{0,1};1+:λp.!G&D=F₁;!P&D=p;&D{(G₀ P₀),!H&E=G₁;!Q&E=P₁;1+&E{(H₀ Q₀),1+(H₁ Q₁)}}}}"
  ]

test :: IO ()
test = forM_ tests $ \ (src, exp) -> do
  env <- new_env (read_book book)
  det <- col env $ read_term src
  det <- show <$> snf env 1 det
  if det == exp then do
    putStrLn $ "[PASS] " ++ src ++ " → " ++ det
  else do
    putStrLn $ "[FAIL] " ++ src
    putStrLn $ "  - expected: " ++ exp
    putStrLn $ "  - detected: " ++ det

main :: IO ()
main = test
