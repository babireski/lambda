module Substitution where

import Type

type Substitution = [(Type, Type)]

(↦) :: Type -> Type -> (Type, Type)
(↦) α β = (α, β)

occurs :: Type -> Type -> Bool
occurs α β = undefined

unify :: Type -> Type -> Substitution
unify α β = undefined 