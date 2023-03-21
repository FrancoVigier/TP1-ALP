module Eval1
  ( eval
 , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm (Skip)             e = skip e
stepComm (IfThenElse b x y) e = let (t :!: e') = evalExp b e 
                                in if t then stepComm x e' else stepComm y e'
stepComm (IfThen b x)       e = let (t :!: e') = evalExp b e 
                                in if t then stepComm x e' else skip e'
stepComm (Repeat x b)       e = stepComm (Seq (x) (IfThenElse b (Skip) (Repeat x b))) e
stepComm (Let n v)          e = let (i :!: e') = evalExp v e
                                in (Skip :!: update n i e')
stepComm (Seq Skip y)       e = stepComm y e
stepComm (Seq x y)          e = let (v :!: e') = (stepComm x e) 
                                in stepComm y e'
skip e = (Skip :!: e)

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State

evalExp (BTrue)    e = (True :!: e)
evalExp (BFalse)   e = (False :!: e)

evalExp (Lt x y) e = evalBinExp (<) x y e
evalExp (Gt x y) e = evalBinExp (>) x y e
evalExp (Eq x y) e = evalBinExp (==) x y e
evalExp (NEq x y) e = evalBinExp (/=) x y e

--TODO cortocircuito?
evalExp (And x y) e = evalBinExp (&&) x y e
evalExp (Or x y) e = evalBinExp (||) x y e


evalExp (Const x) e = (x :!: e)
evalExp (Var x) e = (lookfor x e :!: e)

evalExp (UMinus x) e = let (r :!: e') = (evalExp x e)
                       in (-r :!: e')

evalExp (Plus x y) e = evalBinExp (+) x y e
evalExp (Minus x y) e = evalBinExp (-) x y e
evalExp (Times x y) e = evalBinExp (*) x y e
evalExp (Div x y) e = evalBinExp (div) x y e

evalExp (EAssgn x y) e = let (r :!: re) = evalExp y e
                         in (r :!: update x r re)
evalExp (ESeq x y) e = evalBinExp (\l r -> r) x y e
                          
                          
evalBinExp f x y e = let (r :!: re) = evalExp x e
                         (l :!: le) = evalExp y re
                     in ((f r l) :!: le)
                       
                       
                       
                       
