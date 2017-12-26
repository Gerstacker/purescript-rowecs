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



type CompStorage (c :: Type -> Type) rowS = Record rowS



read :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => CompStorage c rowS -> SProxy name -> Int -> Maybe a
read cstor spr ind = get v ind
  where
    v = (R.get spr cstor) :: c a


unsafeRead :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => CompStorage c rowS -> SProxy name -> Int -> a
unsafeRead cstor spr ind = unsafePartial $ fromJust $ get v ind
  where
    v = (R.get spr cstor) :: c a


write :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => CompStorage c rowS -> SProxy name -> Int -> a -> CompStorage c rowS
write cstor spr ind val = stor'
  where
    intmap = (R.get spr cstor) :: c a
    intmap' = set intmap ind val
    stor' = R.set spr intmap' cstor


class AllocateStorage (c :: Type -> Type) (listD :: RowList)  (rowS :: # Type) a
    | listD c -> rowS a, rowS -> c
  where
    allocateStorageImpl :: RLProxy listD -> Proxy2 c -> CompStorage c rowS

instance allocateStorageNil :: AllocateStorage m Nil () a where
  allocateStorageImpl _ _ = {}

instance allocateStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorage c listD' rowS' b
  ) => AllocateStorage c (Cons name a listD') rowS a where
  allocateStorageImpl _ _ = R.insert nameP allocate rest
    where
      nameP = SProxy :: SProxy name
      rest = allocateStorageImpl (RLProxy :: RLProxy listD') (Proxy2 :: Proxy2 c) :: CompStorage c rowS'

allocateStorage :: forall c rowD listD a rowS
  . RowToList rowD listD
  => AllocateStorage c listD rowS a
  => Storage c a
  => RProxy rowD
  -> Proxy2 c
  -> CompStorage c rowS
allocateStorage _ = allocateStorageImpl (RLProxy :: RLProxy listD)


class ReadStorage (c :: Type -> Type) (rowS :: # Type) (listD :: RowList) (rowD :: # Type) a
    | rowS -> c, listD -> rowD, rowD -> a
  where
    readStorageImpl :: RLProxy listD -> CompStorage c rowS -> Int -> Record rowD

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
  readStorageImpl _ cstor ind = R.insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = unsafeRead cstor nameP ind
      rest = (readStorageImpl (RLProxy :: RLProxy listD') cstor ind) :: Record rowD'
