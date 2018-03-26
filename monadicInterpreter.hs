import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

data Exp = Con Int        -- integer constant
         | Var Var        -- variable
         | Add Exp Exp    -- e1 + e2
         | Mul Exp Exp    -- e1 * e2
         | Sub Exp Exp    -- e1 - e2
         | Div Exp Exp    -- e1 `div` e2 (Integer divison)
         | Neg Exp        -- (-e)
         | PP Var         -- ++v
         | Assign Var Exp -- v = e
         deriving Show

type Var = String
type State = Var -> Int
newtype StM a = SM (State -> (a, State))

uSM :: StM a -> (State -> (a, State))
uSM (SM a) = a

instance Monad StM where
	return x = SM (\y -> (x,y))
	e >>= f = SM (\n -> let (a, y) = uSM e n
		      	        (b, z) = uSM (f a) y
			    in (b,z) )
instance Functor StM where
	fmap f (SM e) = SM (\n -> let (a,n1) = e n
				  in (f a, n1))

instance Applicative StM where
	pure x = return x
	SM f<*>SM a = SM (\n -> let (y, n1) =  f n
				    (z, n2) =  a n1
				in (y z, n2)) 
	
--initial state
ist :: State
ist = \x -> error $ "Variable not defined: " ++ x

--callable function
interpret :: Exp -> Int
interpret e = fst $uSM (eval e) ist

--main function 
eval :: Exp -> StM Int

-- Evaluate Constant
eval (Con i) = return i

-- Evaluate variable
eval (Var v) = SM (\n -> (n v,n))

-- Evaluate Add
eval (Add a b) = eval a >>=
			\i1 -> eval b >>=
				\i2 -> return (i1+i2) 

-- Evaluate Mul
eval (Mul a b) = eval a >>=
                         \i1 -> eval b >>=
                                 \i2 -> return (i1*i2)

-- Evalualte Sub
eval (Sub a b) = eval a >>=
			\i1 -> eval b >>=
				\i2 -> return (i1-i2) 

--Evalualte Div
eval (Div a b) = eval a >>=
			\i1 -> eval b >>=
				\i2 -> if (i2==0) then error "division by zero" 
						  else return (i1`div`i2)

--Evaluate Neg
eval (Neg a) = eval a >>=
			\i1 -> return(-i1) 

--Evaluate Assign
eval (Assign v e) = eval e >>=
				\i1 -> SM (\n -> (i1, (\x -> if (x==v) then i1
								       else n x)))

--Evaluate PP (Plus Plus)
eval (PP v) = eval (Var v) >>=
			\i1 -> SM (\n -> (i1, (\x -> if (x==v) then i1
                                                               else n x)))

                                                         
