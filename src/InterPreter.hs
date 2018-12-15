module InterPreter where
import Data.Map
import InterPreter.Env

data TopOperation =
    Let String |                -- Define variable
    Do String StoreOperation |  -- Do operation
    Print Value |               -- Display variable
    If Value [TopOperation] [TopOperation] |
    While String [TopOperation]
    deriving Read

newtype StoreOperation =
    Is Value
    deriving Read

data Value =
    Number Int |                -- Literal
    Ref String |                -- Reference variable
    Add Value Value |           -- +
    Sub Value Value |           -- -
    Mul Value Value |           -- *
    Div Value Value |           -- /
    Mod Value Value |           -- %
    Eq  Value Value |           -- ==
    Ne  Value Value |           -- !=
    Lt  Value Value |           -- <
    Gt  Value Value |           -- >
    Le  Value Value |           -- <=
    Ge  Value Value             -- >=
    deriving Read

-- extract value
value :: Value -> Env -> Int
value (Number v) _ = v
value (Ref var) env = env ! var
-- 四則演算 + 剰余
value (Add l r) env = value l env + value r env
value (Sub l r) env = value l env - value r env
value (Mul l r) env = value l env * value r env
value (Div l r) env = value l env `div` value r env
value (Mod l r) env = value l env `mod` value r env
-- 比較
value (Eq l r) env  = bool2int $ value l env == value r env
value (Ne l r) env  = bool2int $ value l env /= value r env
value (Lt l r) env  = bool2int $ value l env <  value r env
value (Gt l r) env  = bool2int $ value l env >  value r env
value (Le l r) env  = bool2int $ value l env <= value r env
value (Ge l r) env  = bool2int $ value l env >= value r env

bool2int :: Bool -> Int
bool2int True  = 1
bool2int False = 0

-- execute an operation
exec :: TopOperation -> Env -> IO Env
exec (Let var) env = return $ insert var 0 env   -- Let
exec (Do var (Is v)) env = return $ insert var (value v env) env
exec (Print var) env = print (value var env) >> return env
exec (If var trues falses) env = if value var env /= 0 then execAll trues env else execAll falses env
exec op@(While var ops) env = if env ! var == 0 then return env else next
    where next = execAll ops env >>= exec op

execAll :: [TopOperation] -> Env -> IO Env
execAll [] env = return env
execAll list env = Prelude.foldl (\e o -> e >>= exec o) (return env) list
