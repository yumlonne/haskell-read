module InterPreter where
import Data.Map
import InterPreter.Env

data TopOperation =
    Let String |                -- Define variable
    Do String StoreOperation |  -- Do operation
    Print Value |               -- Display variable
    If Value |
    Else |
    Then TopOperation
    deriving Read

newtype StoreOperation =
    Is Value
    deriving Read

data Value =
    Number Int |                -- Literal
    Ref String |                -- Reference variable
    Add Value Value             -- Add Values
    deriving Read

-- extract value
value :: Value -> Env -> Int
value (Number v) _ = v
value (Ref var) env = env ! var
value (Add v1 v2) env = value v1 env + value v2 env

-- execute an operation
exec :: TopOperation -> Env -> IO Env
exec (Let var) env = return $ insert var 0 env   -- Let
exec (Do var (Is v)) env = return $ insert var (value v env) env
exec (Print var) env = print (value var env) >> return env

execAll :: [TopOperation] -> Env -> IO Env
execAll [] env = return env
execAll list env = Prelude.foldl (\e o -> e >>= exec o) (return env) list
