{-# Language QuantifiedConstraints,MultiParamTypeClasses,FunctionalDependencies,UndecidableSuperClasses,TypeApplications,ConstraintKinds,AllowAmbiguousTypes,ScopedTypeVariables,TypeFamilyDependencies,FlexibleInstances,TypeOperators ,DataKinds , PolyKinds , TypeFamilies , TypeSynonymInstances , GADTs , UndecidableInstances#-}

import To
import HFContainer
import HFList
import HFNonempty
import HFTree
import HFFree
import VFree

{-
--egHFree2' :: HFree2 (HList '[String ,HList '[Int ,Bool]])
--egHFree2' HFree2 (HPure2 "hello" `FCons` ((HFree2 (HPure2 0 `FCons` (HPure2 True `FCons` FEmpty)) `FCons` FEmpty)))

--egHFree2'' :: HFree2 ('VPure [Char])
--egHFree2'' = HFree2 ((FCons::HFree2 ('VPure [Char]) -> FList HFree2 '[] -> FList HFree2 xs2) (HPure2 "hello")  (FEmpty :: FList f0 '[]))

--egHFree2'' :: HFree2 ('VPure [Char])
egHFree2'' = HFree2 (((FEnd :: f0 x0 -> fNonempty f0 ('FEnd x0)) "" :: fList0 HFree2 xs2)) -- (FEnd (VPure "hello")))
 
FUCK NOW THE PROBLEM IS TRYING TO CREATE
	a double F-container
 ie, that it needs to have an FContainer
 instead of the container corresponding to the fcontainer
 as the parameter storing the types to the fcontainer
  because
 there are 2 things being mapped over the contents
namely
 the HFree2 and VFree constructors.


--('VFree ((fList :: (VFree * l0 -> *) -> container (VFree * l0) -> *) (VFreeJ *) xs))
{-
    -> HFree2 ((To (F_Container container k *) 
                   (H_Container container k  ) fList) xs)
-}
-- HPure2 :: (x :: *) -> HFree2 (VPure x)

{--}

{-
    * Couldn't match type `a' with `*'
      `a' is a rigid type variable bound by
        the type signature for:
          eg :: forall a. VFree [Char] ('Just (HList '[ 'Nothing]))
      Expected type: VFree [Char] ('Just (HList '[ 'Nothing]))
        Actual type: VFree
                       [Char]
                       ('Just
                          (To
                             (F_Container [] (Maybe *) *)
                             (H_Container [] (Maybe *))
                             FList
                             '[ 'Nothing]))
    * In the expression: VFree $ VPure "hello" `FCons` FEmpty
      In an equation for `eg': eg = VFree $ VPure "hello" `FCons` FEmpty
    * Relevant bindings include
        eg :: VFree [Char] ('Just (HList '[ 'Nothing]))
    |
121 | eg = VFree $ VPure "hello" `FCons` FEmpty
-}


{-egVFree :: VFree String (Just (HList '[Nothing]))
--egVFree = VFree (HPure "hello" `FCons` ((VFree (VPure "0" `FCons` (VPure "True" `FCons` FEmpty)) `FCons` FEmpty)))
egVFree = VFree $ (VPure "hello") `FCons` FEmpty  -- VFree (VPure "0" `FCons` (VPure "True" `FCons` FEmpty))
-}
--eg :: VFree [Char] ('Just (HList '[ 'Nothing]))
--eg  :: VFree [Char] ('Just (HList '[ 'Nothing]))


-- as they are hetrogenous
-- the containers `f' need to take the same container
-- but the homogeneous version, of type parameters,
-- to indicate the values at each position
-- in the type parametrised hetrgenous container.

-- there might be a problem with the Constraint
-- an instance of which is required of each container
-- in the Tree-like parameter to HFree, that is used to
-- determine its F-equivaluent to pattern match on the types
-- which are mapped over 
-- by the type levelHFree constructor at each layer
-- that is, expanding the type of the arguments passed to the
-- HFree value constructor (that is a polymorphic hList full of
-- hetrogenous types as indicated by a type parameter, which is 
-- this hList type full of types) that this type can be expanded
-- by "unmapping" the HFree and storing this type funciton in an fList.
-- the opposite is taking the type of the input hList of HFree's
-- as it is usually written (using a hList instead of a fList - note
-- polymorphic lower casing of these to show they can change between layers)
-- and then doing the unmapping by mapping over this type to remove
-- the HFree constructors that effectively have been mapped over 
-- this Tree-like parameter that is supposed to be input as the type
-- parameter of the HFree - that is, that;
-- while it takes hLists of HFree's as the type of the
-- argument passed to the HFree value constructor, 
-- the type parameter supplied to HFree indicating its internal types,
-- is that of this input data (argument to HFree at value level) stored
-- without the HFree's interspersing this type, so making it just a
-- a nesting of hFree's. so instead of requiring the type input to be
-- an fList in order to obtain these type arguments to the corresponding
-- polymorphic hList stored as the type parameter to HFree, we instead
-- want to map over this hList full of HFree's in order to obtain the
-- type argument to HFree.
-- this mapping to unwrap the HFree solves the use of fLists, and in 
-- doing so eleviates the demand that the hList type can be obtained
-- from the fList type by some type family that exists as an associated
-- type to a class which the fFlists must instantiate, in order for this
-- type family to be available to them to map from the type of the 
-- fList supplied as the type of the argument to 
-- the HFree value constructor, to obtain the corresponding hList
-- to use in the version of the fList which mapped the HFree constructor
-- which has not had this mapping, namely the hList corresponding to the 
-- fList, which is only different because it does not take a type family
-- to map over the contents, which in this case is the HFree constructor.
-- by mapping to remove the HFree constructor, the input type to 
-- the HFree value constructor (the type of which we are trying to determine)
-- can be left in its hList form and no class is needed for an associated
-- type to be used to determine the type of the hList from the input fList.

-- only takes 1 param, as when compared with Free
-- the type `a' indicates the type of the values,
-- while here they are held in a Tree-like parameter,
-- which has hetrogenous values throughout
-- so contain the types of the values at each position,
-- and the type parameter for the homogenous `a' can be ommited.
--data HFree t where
-- Free :: (f (Free f a)) -> Free f a 
{-*
  HFree :: fList HFree xs -> (HListOf fList) xs
-- really HListOf should be an associated type to a class that the 
-- fList should be constrained to instantiate.

type family HListOf (fList :: (k->*) -> ((HListOf flist) k) -> * )
-}
-- HFree :: hList xs -> HFree (FMap UnHFree xs)
-- oh no! now they have to be constrained to be type level functors!?

--class UnFunctor fList where
-- type HListOf (fList :: (k -> *) -> ((HListOf flist) k) -> * )
{-
class (forall a. HomogenousOf ys a ~ xs a,forall a. ys a ~ HetrogenousOf xs a) => Hetrogenous (xs :: * -> *) (ys :: (* -> *)) | xs -> ys, ys -> xs where
 type HetrogenousOf xs :: * -> *
 type HomogenousOf  ys :: * -> *
-- hList k is wrong, it does not have the leaf type available unless it is only vertically hetrogenous. use hList xs instead
class HomogenousOf hList ~ list => UnFunctor (hList :: list * -> *) (fList :: (k -> * ) -> (hList xs) -> * ) | fList -> hList,hList -> fList where
 type FListOf hList :: (k -> * ) -> (hList xs) -> * 
 type HListOf fList :: hList xs
-}
{-
type family HetrogenousOf (xs :: * -> *) :: xs * -> *
type family HomogenousOf  (xs :: Int) :: * -> * -- (xs :: (HomogenousOf xs) * -> *) :: * -> *
type family FListOf xs :: (k -> *) -> ((HomogenousOf xs) k) -> (HomogenousOf xs) * -- (xs :: (HomogenousOf xs) * -> *) 
type family HListOf (xs :: (k -> *) -> ((HomogenousOf ys) k) -> *) :: ((HomogenousOf xs) *) -> *
-}
{-

data Nonempty a = End a | a :| Nonempty a

data Nesting (containers ) a where
-- Floor :: f a -> Nesting (End f) a
 Nesting :: FList HTree xs -> HTree (Branches xs)
-- f (Nesting fs a) -> Nesting (f :| fs) a

-- vertically hetrogenous tree
--data VTree c (xs :: VTree c ys *) -- = VBranches (c xs)

data NVTree (t :: Tree *) where 
 HBranches :: fList NVTree xs -> HTree (Branches xs)
 NVLeaf     :: HTree (Leaf x) 

-}
-}