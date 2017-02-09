{-# LANGUAGE MultiParamTypeClasses
            , TemplateHaskell
            , ScopedTypeVariables
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
#-}

module Common where

import           Unbound.LocallyNameless


data Operation = Arith ArithOp
               | Logical LogicalOp
               deriving (Eq, Show)


data ArithOp = Add | Sub | Mul | Div deriving (Eq, Show)

data LogicalOp = Lt | Gt | Equ | Neq deriving (Eq, Show)

$(derive [''Operation])
$(derive [''ArithOp])
$(derive [''LogicalOp])

instance Alpha ArithOp
instance Alpha LogicalOp
instance Alpha Operation
