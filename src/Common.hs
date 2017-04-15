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
               | Comp CompOp
               | Logical LogicalOp
               | Append
               deriving (Eq, Show)


data ArithOp = Add | Sub | Mul | Div deriving (Eq, Show)

data CompOp = Lt | Gt | Equ | Neq deriving (Eq, Show)

data LogicalOp =  LAnd | LOr deriving (Eq, Show)

$(derive [''Operation])
$(derive [''ArithOp])
$(derive [''CompOp])
$(derive [''LogicalOp])

instance Alpha ArithOp
instance Alpha LogicalOp
instance Alpha Operation
instance Alpha CompOp
