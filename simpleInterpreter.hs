--simple interpreter written in haskell
--call interpret Exp

data Exp = Con Int -- integer constant
         | Var Var -- variable
         | Add Exp Exp -- e1 + e2
         | Mul Exp Exp -- e1 * e2
         | Sub Exp Exp -- e1 - e2
         | Div Exp Exp -- e1 `div` e2 (Integer divison)
         | Neg Exp -- (-e)
         | PP Var -- ++v
         | Assign Var Exp -- v = e

type Var   = String   -- variables are named "x", "foo", "count1" etc.
type State = Var -> Maybe Int       -- You decide the representation
ist = \_ -> error "variable \"x\" is undefined."

interpret :: Exp -> Int
interpret e = let (a,b) = eval e ist
              in a
              
eval (Con i) s = (i,s)
eval (Var v) s = (s v, s)
eval (Add e1 e2) s = let (a1, s1) = eval e1 s
                         (a2, s2) = eval e2 s1
                     in (a1 + a2, s2)

eval (Mul e1 e2) s = let (a1, s1) = eval e1 s
                         (a2, s2) = eval e2 s1
                     in (a1 * a2, s2)

eval (Sub e1 e2) s = let (a1, s1) = eval e1 s
                         (a2, s2) = eval e2 s1
                     in (a1 - a2, s2)

eval (Div e1 e2) s = let (a1, s1) = eval e1 s
                         (a2, s2) = eval e2 s1
                     in (a1 `div` a2, s2)

eval (Neg e1) s = let (a1, s1) = eval e1 s
                  in (-a1, s1)

eval (Assign v e1) s = let (a1, s1) = eval e1 s
                           s2 = \v' -> if (v == v') then a1 else s1 v'
                               in (a1, s2)
eval (PP v) s =  let a1 = s v 
                     s1 = \v' -> if (v' == v) then (a1+1) else s v'
                 in (1+a1,s1)
