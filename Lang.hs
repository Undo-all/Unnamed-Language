module Lang where

import Control.Monad.Except
import Data.Map.Strict (Map)
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

data Expr = LInt Int
          | LReal Double
          | Ident String
          | Apply Expr [Expr]
          | Define String [String] [Expr]
          deriving (Eq, Show)

type Env = Map String Value

data Error = NotFound String
           | TooManyArgs String
           | AloneCurry (Maybe String)
           deriving (Eq, Show)

type Eval a = StateT Env (Except Error) a

data Value = Int Int
           | Real Double
           | Prim (Env -> [Value] -> Eval Value)
           | Func (Maybe String) Int [String] [Expr]

instance Show Value where
    show (Int n) = show n
    show (Real d) = show d
    show (Prim p) = "<primative>"
    show (Func Nothing _ args _) =
        "<anonymous function of " ++ unwords args ++ ">"
    show (Func (Just name) _ args _) =
        "<function " ++ name ++ " of " ++ unwords args ++ ">"

