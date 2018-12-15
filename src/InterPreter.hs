module InterPreter where

data TopOperation =
    Let String |                -- Define variable
    Do String StoreOperation |  -- Do operation
    Print String                -- Display variable
    deriving Read

data StoreOperation =
    Is Value
    deriving Read

data Value =
    Number Int |                -- Literal
    Ref String |                -- Reference variable
    Add Value Value             -- Add Values
    deriving Read
