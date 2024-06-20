module Unifier where

import Type

class Unifiable a where
    unify :: a -> a -> Maybe [(a, a)]

instance Unifiable Type where
    unify :: Type -> Type -> Maybe [(Type, Type)]
    unify = undefined

instance Unifiable Typescheme where
    unify :: Typescheme -> Typescheme -> Maybe [(Typescheme, Typescheme)]
    unify = undefined
