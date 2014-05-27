import Control.Monad
import Pipes.Internal

(+++) :: [a] -> [a] -> [a]
[] +++ l = l
(x : xs) +++ l = x : (xs +++ l)

(>==) :: Monad m => Proxy a a' b b' m x -> (x -> Proxy a a' b b' m r) -> Proxy a a' b b' m r
Pure x        >== p  = p x
Request a ca' >== p  = Request a (\a' -> ca' a' >== p)
Respond b' cb >== p  = Respond b' (\b -> cb b >== p)
M cm          >== p  = M (liftM (>== p) cm)
