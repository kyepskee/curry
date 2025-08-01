{
module Parser where

import Data.Char
}

%name parse Program
%tokentype { Token }
%error { parseError }


%token 
    sort  { TokenSort }
    int   { TokenInt }
    '<='  { TokenSubtype }
    ':'   { TokenColon }
    '->'  { TokenArrow }
    ident { TokenIdent $$ }

%left '->'
%%

Program : Exp         { [$1] }
        | Exp Program { $1 : $2 }

Exp : SortDecl { SortDecl $1 }
    | TypeDecl { TypeDecl $1 }

SortDecl : sort ident ':' int { NewSort $2 }
         | sort ident '<=' ident { Subsort $2 $4}

TypeDecl : ident ':' TypeExp { Decl $1 $3 }

TypeExp : ident                { TypeIdent $1 }
        | TypeExp '->' TypeExp { TypeArrow $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
    = SortDecl Sort
    | TypeDecl TypeDecl
    deriving (Show)

data Sort 
    = NewSort String
    | Subsort String String
    deriving (Show)

data Type
    = TypeIdent String
    | TypeArrow Type Type
    deriving (Show)

data TypeDecl = Decl String Type
    deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
lexer ('<':'=':cs) = TokenSubtype : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs

data Token 
    = TokenInt
    | TokenSort
    | TokenSubtype
    | TokenArrow
    | TokenColon
    | TokenIdent String
    deriving (Show)

lexVar cs =
    case span isAlpha cs of
        ("int", rest)  -> TokenInt : lexer rest
        ("sort", rest) -> TokenSort : lexer rest
        (var, rest)    -> TokenIdent var : lexer rest
}
