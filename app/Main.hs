{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Foldable
import Data.Maybe
import Control.Exception
import Debug.Trace

import Control.Lens ((&), (^.), (%~), to, at, (^?), ix)
import qualified Control.Lens as L

import qualified Parser as P

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

iota :: Type
iota = Base Iota

data TypingException 
    = EnvLength
    | NotAtomic
    deriving (Show)
instance Exception TypingException

data Env = Env
    { _types :: [Type]
    }
    deriving (Eq,Show)

L.makeLenses ''Env

typecheck :: Env -> Term -> Maybe Type
typecheck env (Lam t term) = do
    t' <- typecheck (env & types %~ (t:)) term
    pure $ LamT t t'
typecheck _ (Const t) = Just t
typecheck env (Var idx len) =
    if (env ^. types . to length) == len then
        env ^? types . ix idx
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

data Cutoff = AtLeast Int

shift :: Term -> Int -> Cutoff -> Term
shift (Const cst) _ _ = (Const cst)
shift (Var n len) k (AtLeast c) =
    if n >= c then
        Var (n + k) (len + k) -- FIXME
    else
        Var n (len + k)
shift (App t0 t1) k c =
    App (shift t0 k c) (shift t1 k c)
shift (Lam t term) k (AtLeast c) =
    Lam t (shift term k (AtLeast (c + 1)))

shiftAll :: Term -> Int -> Term
shiftAll term by =
    shift term by (AtLeast 0)

expand :: Typed -> Term
expand (Typed _ term (Base _)) = term
expand (Typed env term (LamT s t)) =
    Lam s expanded
    where
        body = 
            App (shiftAll term 1) 
                (Var 0 (env^.types.to length + 1))
        expanded = 
            expand (Typed (env & types %~ (s:)) body t)

ass :: String -> Maybe a -> IO a
ass _ (Just x) = pure x
ass str Nothing = fail str

termHead :: Term -> Term
termHead (Lam _ _) = error "not an atomic term"
termHead (App r _) = termHead r
termHead x = x

substRR :: Term -> (Int, Term) -> Term
substRR (App r n) (x, m) =
    App (substRR r (x, m))
        (substN n (x, m))
substRR (Const c) _ = Const c
substRR (Var x len) (x', n) =
    if x == x' then
        error "replacing variable turns atomic into canonical"
    else
        (Var x len)
substRR (Lam _ _) _ = error "not an atomic term"

substRN :: Term -> (Int, Term) -> Term
substRN (Const c) _ = Const c
substRN (Var x len) (x', m) =
    if trace ((show x) ++ " = " ++ (show x'))(x == x') then
        trace ("m = " ++ show m) m
    else
        error "not replacing variable turns into atomic"
substRN (Lam _ _) _ = error "not an atomic term"
substRN (App r n) sub =
    shiftAll (substN n' (0, m')) (-1)
    where
        (n', t) = case substRN r sub of
                Lam t n' -> (n', t)
                _ -> error "substRN returned atomic"
        
        m' = shiftAll (substN n sub) 1


substN :: Term -> (Int, Term) -> Term
substN (Lam t n) (x, m) = Lam t (substN n (x+1, (shiftAll m 1)))
substN r (x, m) =
    case traceShowId (termHead r) of
        Var x' _ | x == (traceShowId x') ->
            substRN r (x, m)
        _ ->
            substRR r (x, m)

emptyEnv = Env []

main :: IO ()
main = do
    putStrLn . show $ P.parse $ P.lexer "sort nat : int\nsort zero <= nat\nsort pos <= nat\nz : zero"
    putStrLn . show . typecheck emptyEnv $ 
        (Lam (LamT iota iota)
            (Lam iota
                (App (Var 1 2) (App (Var 1 2) (Var 0 2)))))
    typed <- ass "typeTerm failed" $ typeTerm (Env [LamT iota iota]) (Var 0 1)
    putStrLn . show $ typed
    putStrLn . show $ expand typed

    let env = Env [LamT iota (LamT iota iota), LamT iota (LamT iota iota)]
    let replaceTerm = Lam iota 
            (Lam iota 
                (App (App (Var 2 4) (Var 0 4)) (Var 0 4)))
    putStrLn . show $ typeTerm env replaceTerm
    let subbing = expand . fromJust $ typeTerm env (Var 1 2)
    putStrLn . show $ subbing
    let subbed = substN replaceTerm (0, expand . fromJust $ typeTerm env (Var 1 2))
    putStrLn . show $ subbed
    putStrLn . show $ typeTerm env subbed
