module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado nulo
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (m, s) = lookfor' (m M.!? v)

lookfor' (Just x)  = Right x
lookfor' (Nothing) = Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update x v (m, s) = (M.insert x v m, s)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace s (m, t) = (m, t ++ s)  

addVarTrace :: Variable -> Int -> State -> State
addVarTrace v i e = addTrace ("Let " ++ v ++ " " ++ show i ++ "\n") e

tracingUpdate v i e = addVarTrace v i (update v i e)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Skip)             e = skip e
stepComm (IfThenElse b x y) e = evalFailingExp (evalExp b e) (\(t :!: e') -> if t then stepComm x e' else stepComm y e')
stepComm (IfThen b x)       e = evalFailingExp (evalExp b e) (\(t :!: e') -> if t then stepComm x e' else skip e')
stepComm (Repeat x b)       e = stepComm (Seq (x) (IfThenElse b (Skip) (Repeat x b))) e
stepComm (Let n v)          e = evalFailingExp (evalExp v e) (\(i :!: e') -> Right  (Skip :!: tracingUpdate n i e'))
stepComm (Seq Skip y)       e = stepComm y e
stepComm (Seq x y)          e = evalFailingExp (stepComm x e) (\(v :!: e') -> stepComm y e')
skip e = Right (Skip :!: e)

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)

evalExp (BTrue)    e = Right (True :!: e)
evalExp (BFalse)   e = Right (False :!: e)

evalExp (Lt x y) e = evalBinExp (<) x y e
evalExp (Gt x y) e = evalBinExp (>) x y e
evalExp (Eq x y) e = evalBinExp (==) x y e
evalExp (NEq x y) e = evalBinExp (/=) x y e

--TODO cortocircuito?
evalExp (And x y) e = evalBinExp (&&) x y e
evalExp (Or x y) e = evalBinExp (||) x y e


evalExp (Const x) e = Right (x :!: e)
evalExp (Var x) e =  evalFailingExp (lookfor x e) (\x -> Right (x :!: e))
                    

evalExp (UMinus x) e = evalFailingExp (evalExp x e) (\(r :!: e') -> Right (-r :!: e))

evalExp (Plus x y) e = evalBinExp (+) x y e
evalExp (Minus x y) e = evalBinExp (-) x y e
evalExp (Times x y) e = evalBinExp (*) x y e
evalExp (Div x y) e = evalBinExp (div) x y e

evalExp (EAssgn x y) e = let value = (evalExp y e)
                             f (r :!: re) = Right (r :!: tracingUpdate x r re)
                         in evalFailingExp value f
evalExp (ESeq x y) e = evalBinExp (\l r -> r) x y e
                          
                          
evalBinExp f x y e = let h r (l :!: le) = Right ((f r l) :!: le)
                         g (r :!: re) = evalFailingExp (evalExp y re) (h r)
                     in evalFailingExp (evalExp x e) g

evalFailingExp :: Either Error a -> (a -> Either Error b) -> Either Error b
evalFailingExp exp f = case exp of
                       Right x -> (f x)
                       Left y  -> Left y
