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


class HasComponent cs (c :: Type -> Type) (rowS :: # Type) (name :: Symbol) a
    | cs -> c, rowS name -> a, c rowS -> cs

instance csHC ::
  ( Storage c a
  , IsSymbol name
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  ) => HasComponent (CompStorage c rowS) c rowS name a




newtype CompStorage (c :: Type -> Type) (rowS  :: # Type) = CompStorage (Record rowS)

unCS :: forall rowS c . CompStorage c rowS -> Record rowS
unCS (CompStorage rec) = rec


read :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => CompStorage c rowS -> SProxy name -> Int -> Maybe a
read (CompStorage csrec) spr ind = get v ind
  where
    v = (R.get spr csrec) :: c a


unsafeRead :: forall rowS name a c rowS'
{-  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS -}
  . HasComponent (CompStorage c rowS) c rowS name a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => Storage c a
  => CompStorage c rowS -> SProxy name -> Int -> a
unsafeRead (CompStorage csrec) spr ind = unsafePartial $ fromJust $ get v ind
  where
    v = (R.get spr csrec) :: c a


write :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => CompStorage c rowS -> SProxy name -> Int -> a -> CompStorage c rowS
write (CompStorage csrec) spr ind val = CompStorage stor'
  where
    intmap = (R.get spr csrec) :: c a
    intmap' = set intmap ind val
    stor' = R.set spr intmap' csrec


class AllocateStorage (c :: Type -> Type) (listD :: RowList)  (rowS :: # Type) a
    | listD c -> rowS a, rowS -> c
  where
    allocateStorageImpl :: RLProxy listD -> Proxy2 c -> CompStorage c rowS

instance allocateStorageNil :: AllocateStorage m Nil () a where
  allocateStorageImpl _ _ = CompStorage {}

instance allocateStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorage c listD' rowS' b
  ) => AllocateStorage c (Cons name a listD') rowS a where
  allocateStorageImpl _ _ = CompStorage $ R.insert nameP allocate $ unCS rest
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
  , RowCons name a rowD' rowD
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowD'
  , HasComponent (CompStorage c rowS) c rowS name a
  , ReadStorage c rowS listD' rowD' b
  ) => ReadStorage c rowS (Cons name a listD') rowD a where
  readStorageImpl _ cstor ind = R.insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = unsafeRead cstor nameP ind :: a
      rest = readStorageImpl (RLProxy :: RLProxy listD') cstor ind

readStorage :: forall c rowD rowS listD a
  . RowToList rowD listD
  => ReadStorage c rowS listD rowD a
  => RProxy rowD
  -> CompStorage c rowS
  -> Int
  -> Record rowD
readStorage _ cstor ind = readStorageImpl (RLProxy :: RLProxy listD) cstor ind

{-
class WriteStorage (c :: Type -> Type) (rowS :: # Type) (listD :: RowList) (rowD :: # Type) a
    | rowS -> c, listD -> rowD, rowD -> a
  where
    writeStorageImpl :: RLProxy listD -> CompStorage c rowS -> Int -> Record rowD -> CompStorage c rowS

instance writeStorageNil :: WriteStorage c rowS Nil () a where
   writeStorageImpl _ _ _ = {}

instance writeStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name a rowD' rowD
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowD'
  , HasComponent (CompStorage c rowS) c rowS name a
  , WriteStorage c rowS listD' rowD' b
  ) => WriteStorage c rowS (Cons name a listD') rowD a where
  writeStorageImpl _ cstor ind vals = R.insert nameP val rest
    where
      nameP = SProxy :: SProxy name
      val = unsafeRead cstor nameP ind :: a
      rest = (writeStorageImpl (RLProxy :: RLProxy listD') cstor ind vals') :: Record rowD'

writeStorage :: forall c rowD rowS listD a
  . RowToList rowD listD
  => WriteStorage c rowS listD rowD a
  => RProxy rowD
  -> CompStorage c rowS
  -> Int
  -> Record rowD
writeStorage _ cstor ind = writeStorageImpl (RLProxy :: RLProxy listD) cstor ind
-}
