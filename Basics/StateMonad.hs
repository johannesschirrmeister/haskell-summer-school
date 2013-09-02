module StateMonad where

newtype State s a = State { runState :: s -> (a, s)}
instance Monad (State s) where
        return a = State (\s -> (a, s))
        sa >>= f = State $ \s -> let (a2, s2) = runState sa s
                                 in runState (f a2) s2
                                 
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
