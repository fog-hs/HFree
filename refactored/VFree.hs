{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

module VFree where
import To
import HFContainer
import HFList
import HFNonempty

-- VFree
-- want to recover the parameter of some kind to specify the type of the leafs,
-- then can write HFree using this parameter as Type, to specify the type of the leafs. 
-- this currently is implicit, since to make the HFree for the type param it needs to be *? could be k!?
--
-- while this takes a f of (* -> *)

-- cant turn this into Maybe l ?

-- need VFree * . Just
-- but cant have partially applied type synonym
-- dont what to wrap into newtype either

data VFree a (k :: Maybe *) where
 VPure :: a -> VFree a Nothing
 VFree :: (fList :: (Maybe * -> *) -> container (Maybe *) -> *) (VFree a) xs    
       -> VFree a (Just ((To (F_Container container (Maybe *) *) 
                             (H_Container container (Maybe *)) fList) xs))

eg :: VFree [Char] ('Just ((HList :: [Maybe *] -> *) '[ 'Nothing]))
eg = VFree $ VPure "hello" `FCons` (FEmpty :: FList (VFree [Char]) '[]) 

type VFreeJ a k = VFree a (Just k)

data HFree2 (k :: VFree * l) where
 HFree2 :: fList HFree2 xs -> HFree2 ('VFree xs)
{-
(fList :: ((VFree * ('Just(VFree
                                  *
                                  ('Just
                                     (To
                                        (F_Container container0 (Maybe *) *)
                                        (H_Container container0 (Maybe *))
                                        fList0
                                        xs0))))) -> *) -> fList0 (VFree *) xs0 -> *) HFree2 xs
-}
 HPure2 :: (x :: *) -> HFree2 (VPure x)

egHFree2' :: HFree2 ('VPure [Char])
egHFree2' = HPure2 "hello" 
