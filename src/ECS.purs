module ECS where

import Data.Maybe

import Data.IntMap as IM
import Data.Record (insert, get, set) as R
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy), RProxy)
import Type.Proxy (Proxy2(Proxy2))
import Type.Row (Cons, Nil, kind RowList)

class Storage (c :: Type -> Type) a where
  allocate :: c a
  get :: c a -> Int -> Maybe a
  set :: c a -> Int -> a -> c a


instance storageIntMap :: Storage IM.IntMap a where
  allocate = IM.empty
  get im ind = IM.lookup ind im
  set im ind val = IM.insert ind val im



type ECS (c :: Type -> Type) rowS = (Record (storage :: (Record rowS), nextID :: Int))


read :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => ECS c rowS -> SProxy name -> Int -> Maybe a
read ecs spr ind = get v ind
  where
    v = (R.get spr ecs.storage) :: c a

write :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => ECS c rowS -> SProxy name -> Int -> a -> ECS c rowS
write ecs spr ind val = ecs { storage = stor' }
  where
    intmap = (R.get spr ecs.storage) :: c a
    intmap' = set intmap ind val
    stor' = R.set spr intmap' ecs.storage



class AllocateStorage (c :: Type -> Type) (rowDg :: # Type) (listD :: RowList)  (rowS :: # Type) a
    | listD c -> rowDg rowS a, rowS -> c rowDg
  where
    allocateStorageImpl :: RLProxy listD -> Proxy2 c -> ECS c rowS

instance allocateStorageNil :: AllocateStorage m rowDg Nil () a where
  allocateStorageImpl _ _ = { storage : {}, nextID : 0}

instance allocateStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorage c rowD listD' rowS' b
  ) => AllocateStorage c rowDg (Cons name a listD') rowS a where
  allocateStorageImpl _ _ =
    (rest { storage = stor})
    where
      stor = R.insert nameP val rest.storage
      nameP = SProxy :: SProxy name
      val = allocate
      rest = allocateStorageImpl (RLProxy :: RLProxy listD') (Proxy2 :: Proxy2 c) :: ECS c rowS'

allocateStorage :: forall c rowD listD a rowS
  . RowToList rowD listD
  => AllocateStorage c rowD listD rowS a
  => Storage c a
  => RProxy rowD
  -> Proxy2 c
  -> ECS c rowS
allocateStorage _ = allocateStorageImpl (RLProxy :: RLProxy listD)

--class ReadStorage (c :: Type -> Type) (rowSg :: # Type) (rowDg :: # Type) (listD :: RowList) (rowS :: # Type) a
