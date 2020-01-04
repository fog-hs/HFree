{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

module HFFree where

import To
import HFContainer
import HFList
import HFNonempty


-- Free and Free'

data Free f a where
 Free :: (f (Free f a)) -> Free f a 
 Pure :: a -> Free f a 

data Free' f a where
 Free' :: (f (Free f a)) -> Free' f a
 Pure' :: f a -> Free' f a 

-- HFree wraps the result of unmapping the HFree constructor from the HFrees passed as an argument.
data HFree (k :: z) where
 HFree :: (fList :: (k -> *) -> container k -> *) HFree xs    
       -> HFree ((To (F_Container container k *) 
                     (H_Container container k) fList) xs)
 HPure :: (x :: *) -> HFree x

-- this example only has one type of container to use
egHFree :: HFree (HList '[String ,HList '[Int ,Bool]])
egHFree = HFree (HPure "hello" `FCons` ((HFree (HPure 0 `FCons` (HPure True `FCons` FEmpty)) `FCons` FEmpty)))

-- 

-- same thing now works with Nonempty
egHFree2   :: HFree (HNonempty (String ':| 'End (HNonempty (Integer ':| 'End Bool))))
egHFree2 = HFree (HPure "hello" `FCons' ` (FEnd ((HFree (HPure 0 `FCons' ` (FEnd (HPure True)))))))

-- now can write a mixed layer example
egHFree3   :: HFree (HNonempty (String ':| 'End (HList '[Int ,Bool])))
egHFree3 = HFree (HPure "hello" `FCons' ` (FEnd ((HFree (HPure 0 `FCons` ((HPure True) `FCons` FEmpty))))))
