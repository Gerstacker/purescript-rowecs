module ECS where

import Data.Maybe

import Data.IntMap as IM
import Data.Record (insert, get, set) as R
import Partial.Unsafe (unsafePartial)
import Prelude (($))
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


unsafeRead :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => ECS c rowS -> SProxy name -> Int -> a
unsafeRead ecs spr ind = unsafePartial $ fromJust $ get v ind
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


class AllocateStorage (c :: Type -> Type) (listD :: RowList)  (rowS :: # Type) a
    | listD c -> rowS a, rowS -> c
  where
    allocateStorageImpl :: RLProxy listD -> Proxy2 c -> ECS c rowS

instance allocateStorageNil :: AllocateStorage m Nil () a where
  allocateStorageImpl _ _ = { storage : {}, nextID : 0}

instance allocateStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorage c listD' rowS' b
  ) => AllocateStorage c (Cons name a listD') rowS a where
  allocateStorageImpl _ _ =
    (rest { storage = stor})
    where
      stor = R.insert nameP val rest.storage
      nameP = SProxy :: SProxy name
      val = allocate
      rest = allocateStorageImpl (RLProxy :: RLProxy listD') (Proxy2 :: Proxy2 c) :: ECS c rowS'

allocateStorage :: forall c rowD listD a rowS
  . RowToList rowD listD
  => AllocateStorage c listD rowS a
  => Storage c a
  => RProxy rowD
  -> Proxy2 c
  -> ECS c rowS
allocateStorage _ = allocateStorageImpl (RLProxy :: RLProxy listD)


class ReadStorage (c :: Type -> Type) (rowS :: # Type) (listD :: RowList) (rowD :: # Type) a
    | rowS -> c a, listD -> rowD
  where
    readStorageImpl :: RLProxy listD -> ECS c rowS -> Int -> Record rowD

instance readStorageNil :: ReadStorage c rowS Nil () a where
  readStorageImpl _ _ _ = {}

instance readStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowToList rowD listD
  , RowCons name a rowD' rowD
  , RowLacks name rowD'
  , ReadStorage c rowS listD' rowD' a
  ) => ReadStorage c rowS (Cons name a listD') rowD a where
  readStorageImpl _ ecs ind = R.insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = unsafeRead ecs nameP ind
      rest = (readStorageImpl (RLProxy :: RLProxy listD') ecs ind) :: Record rowD'
