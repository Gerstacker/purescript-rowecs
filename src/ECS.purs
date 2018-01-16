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
  del :: c a -> Int -> c a

instance storageIntMap :: Storage IM.IntMap a where
  allocate = IM.empty
  get im ind = IM.lookup ind im
  set im ind val = IM.insert ind val im
  del im ind = IM.delete ind im


newtype CompStorage (rowS  :: # Type) = CompStorage (Record rowS)

unCS :: forall rowS . CompStorage rowS -> Record rowS
unCS (CompStorage rec) = rec




read :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => CompStorage rowS -> SProxy name -> Int -> Maybe a
read (CompStorage csrec) spr ind = get v ind
  where
    v = (R.get spr csrec) :: c a


unsafeRead :: forall rowS name a c rowS'
  . IsSymbol name
  => RowCons name (c a) rowS' rowS
  => Storage c a
  => CompStorage rowS -> SProxy name -> Int -> a
unsafeRead (CompStorage csrec) spr ind = unsafePartial $ fromJust $ get v ind
  where
    v = (R.get spr csrec) :: c a


write :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => CompStorage rowS -> SProxy name -> Int -> a -> CompStorage rowS
write (CompStorage csrec) spr ind val = CompStorage stor'
  where
    intmap = (R.get spr csrec) :: c a
    intmap' = set intmap ind val
    stor' = R.set spr intmap' csrec

class AllocateStorage (listS :: RowList) (rowS :: # Type) (c :: Type -> Type) a
    | listS -> c a, listS -> rowS
  where
    allocateStorageImpl :: RLProxy listS -> CompStorage rowS

instance allocateStorageNil :: AllocateStorage Nil () c a where
  allocateStorageImpl _ = CompStorage {}

instance allocateStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorage listS' rowS' d b
  ) => AllocateStorage (Cons name (c a) listS') rowS c a
    where
      allocateStorageImpl _ = CompStorage $ R.insert nameP allocate $ unCS rest
        where
          nameP = SProxy :: SProxy name
          rest = allocateStorageImpl (RLProxy :: RLProxy listS') :: CompStorage rowS'

allocateStorage :: forall listS rowS c a
  . RowToList rowS listS
  => AllocateStorage listS rowS c a
  => Storage c a
  => RProxy rowS
  -> CompStorage rowS
allocateStorage _ = allocateStorageImpl (RLProxy :: RLProxy listS)

class AllocateStorageUniform (c :: Type -> Type) (listD :: RowList)  (rowS :: # Type) a
    | listD c -> rowS a, rowS -> c
  where
    allocateStorageUniformImpl :: RLProxy listD -> Proxy2 c -> CompStorage rowS

instance allocateStorageUniformNil :: AllocateStorageUniform m Nil () a where
  allocateStorageUniformImpl _ _ = CompStorage {}

instance allocateStorageUniformCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorageUniform c listD' rowS' b
  ) => AllocateStorageUniform c (Cons name a listD') rowS a where
  allocateStorageUniformImpl _ _ = CompStorage $ R.insert nameP allocate $ unCS rest
    where
      nameP = SProxy :: SProxy name
      rest = allocateStorageUniformImpl (RLProxy :: RLProxy listD') (Proxy2 :: Proxy2 c) :: CompStorage rowS'

allocateStorageUniform :: forall c rowD listD a rowS
  . RowToList rowD listD
  => AllocateStorageUniform c listD rowS a
  => Storage c a
  => RProxy rowD
  -> Proxy2 c
  -> CompStorage rowS
allocateStorageUniform _ = allocateStorageUniformImpl (RLProxy :: RLProxy listD)


class ReadStorage(rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type)  a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    readStorageImpl :: RLProxy listD -> CompStorage rowS -> Int -> Record rowD

instance readStorageNil :: ReadStorage rowS Nil () c a where
   readStorageImpl _ _ _ = {}

instance readStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name a rowD' rowD
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowD'
--  , HasComponent rowS name a
  , ReadStorage rowS listD' rowD' d b
  ) => ReadStorage rowS (Cons name a listD') rowD c a
    where
      readStorageImpl _ cstor ind = R.insert nameP val rest
        where
          nameP = SProxy :: SProxy name
          val = unsafeRead cstor nameP ind :: a
          rest = readStorageImpl (RLProxy :: RLProxy listD') cstor ind

readStorage :: forall c rowD rowS listD a
  . RowToList rowD listD
  => ReadStorage rowS listD rowD c a
  => CompStorage rowS
  -> Int
  -> Record rowD
readStorage cstor ind = readStorageImpl (RLProxy :: RLProxy listD) cstor ind
