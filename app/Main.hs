module Main where

data Name = Name String
    deriving (Eq, Show)

data Term 
    = Lam Name Term
    | Const
    | Var Name
    | App Term Term
    deriving (Eq, Show)

freeVars :: [Name] -> Term -> [Name]
freeVars _ Const = []
freeVars env (Var name) =
    if name `elem` env 
    then []
    else [name]
freeVars env (Lam name term) = 
    freeVars (name : env) term
freeVars env (App t0 t1) =
    freeVars env t0 ++ freeVars env t1

main :: IO ()
main = 
    putStrLn . show $ freeVars [] (Lam x (Lam y (App (Var x) (App (Var y) (Var z)))))
    where
        x = Name "x"
        y = Name "y"
        z = Name "z"
