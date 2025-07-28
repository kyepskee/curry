module Main where

import Data.Foldable
import Control.Exception
import Debug.Trace

data Name = Name String
    deriving (Eq, Show)

data BaseType = Iota
    deriving (Eq, Show)

data Type 
    = LamT Type Type
    | Base BaseType
    deriving (Eq, Show)

data Term 
    = Lam Type Term
    | Const Type
    | Var Int Int
    | App Term Term
    deriving (Eq, Show)

-- freeVars :: [Name] -> Term -> [Name]
-- freeVars _ (Const _) = []
-- freeVars env (Var name _) =
--     if name `elem` env 
--     then []
--     else [name]
-- freeVars env (Lam name _ term) = 
--     freeVars (name : env) term
-- freeVars env (App t0 t1) =
--     freeVars env t0 ++ freeVars env t1

type Env = [Type]

iota :: Type
iota = Base Iota

data TypingException 
    = EnvLength
    | NotAtomic
    deriving (Show)
instance Exception TypingException

typecheck :: Env -> Term -> Maybe Type
typecheck env (Lam t term) = do
    t' <- typecheck (t:env) term
    pure $ LamT t t'
typecheck _ (Const t) = Just t
typecheck env (Var idx len) =
    if length env == len then
        Just $ env !! idx
    else
        throw EnvLength
typecheck env (App term0 term1) =
    case (t0, t1) of
        (Just (LamT a b), Just a') | a == a' -> Just b
        _ -> Nothing
    where
        t0 = typecheck env term0
        t1 = typecheck env term1

data Typed = Typed Env Term Type
    deriving (Eq,Show)

typeTerm :: Env -> Term -> Maybe Typed
typeTerm env term =
    fmap (\t -> Typed env term t) (typecheck env term)

data InfiniteList a = Cons a (InfiniteList a)

-- fresh :: Term -> Name
-- fresh term =
--     go 0
--     where
--         go :: Int -> Name
--         go n =
--             let name = Name ("x_" ++ show n) in
--             if name `elem` (freeVars [] term) then 
--                 go (n+1)
--             else name

shift :: Term -> Int -> Term
shift term by =
    go term by 0
    where
        go (Const cst) k c = (Const cst)
        go (Var n len) k c =
            if n >= c then
                Var (n + k) (len + k) -- FIXME
            else
                Var n (len + k)
        go (App t0 t1) k c =
            App (go t0 k c) (go t1 k c)
        go (Lam t term) k c =
            Lam t (go term k (c + 1))

expand :: Typed -> Term
expand (Typed _ term (Base _)) = term
expand (Typed env term (LamT s t)) =
    Lam s expanded
    where
        body = 
            App (shift term 1) 
                (Var 0 (length env + 1))
        expanded = 
            expand (Typed (s:env) body t)

termHead :: Term -> Term
termHead (Lam _ _) = throw NotAtomic

ass :: String -> Maybe a -> IO a
ass _ (Just x) = pure x
ass str Nothing = fail str

main :: IO ()
main = do
    putStrLn . show . typecheck [] $ 
        (Lam (LamT iota iota)
            (Lam iota
                (App (Var 1 2) (App (Var 1 2) (Var 0 2)))))
    typed <- ass "chujowo" $ typeTerm [LamT iota iota] (Var 0 1)
    putStrLn . show $ typed
    putStrLn . show $ expand typed
