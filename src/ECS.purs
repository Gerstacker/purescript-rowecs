module ECS where

import Data.Array as A
import Data.Foldable (foldl)
import Data.IntMap as IM
import Data.Maybe (Maybe, fromJust)
import Data.Record (insert, get, set, delete) as R
import Data.Ord (compare)
import Data.Ordering (Ordering(..))
import Partial.Unsafe (unsafePartial)
import Prelude (($), (<>), show, class Show)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy), RProxy(RProxy))
import Type.Proxy (Proxy2(Proxy2))
import Type.Row (Cons, Nil, kind RowList)

class Storage (c :: Type -> Type) a where
  allocate :: c a
  get :: c a -> Int -> Maybe a
  set :: c a -> Int -> a -> c a
  del :: c a -> Int -> c a
  indices :: c a -> Array Int
  size :: c a -> Int
  member :: c a -> Int -> Boolean
  merge :: c a -> c a -> c a

instance storageIntMap :: Storage IM.IntMap a where
  allocate = IM.empty
  get im ind = IM.lookup ind im
  set im ind val = IM.insert ind val im
  del im ind = IM.delete ind im
  indices im = IM.indices im
  size im = IM.size im
  member im ind = IM.member ind im
  merge = IM.unionRight

newtype CompStorage (rowS  :: # Type) = CompStorage (Record rowS)

read :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => Record rowS -> SProxy name -> Int -> Maybe a
read csrec spr ind = get v ind
  where
    v = (R.get spr csrec) :: c a


unsafeRead :: forall rowS name a c rowS'
  . IsSymbol name
  => RowCons name (c a) rowS' rowS
  => Storage c a
  => Record rowS -> SProxy name -> Int -> a
unsafeRead csrec spr ind = unsafePartial $ fromJust $ get v ind
  where
    v = (R.get spr csrec) :: c a


write :: forall rowS name a c rowS'
  . Storage c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => Record rowS -> SProxy name -> Int -> a -> Record rowS
write csrec spr ind val = stor'
  where
    intmap = (R.get spr csrec) :: c a
    intmap' = set intmap ind val
    stor' = R.set spr intmap' csrec

class AllocateStorage (listS :: RowList) (rowS :: # Type) (c :: Type -> Type) a
    | listS -> c a, listS -> rowS
  where
    allocateStorageImpl :: RLProxy listS -> Record rowS

instance allocateStorageNil :: AllocateStorage Nil () c a where
  allocateStorageImpl _ = {}

instance allocateStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorage listS' rowS' d b
  ) => AllocateStorage (Cons name (c a) listS') rowS c a
    where
      allocateStorageImpl _ = R.insert nameP allocate rest
        where
          nameP = SProxy :: SProxy name
          rest = allocateStorageImpl (RLProxy :: RLProxy listS') :: Record rowS'

allocateStorage :: forall listS rowS c a
  . RowToList rowS listS
  => AllocateStorage listS rowS c a
  => Storage c a
  => RProxy rowS
  -> CompStorage rowS
allocateStorage _ = CompStorage $ allocateStorageImpl (RLProxy :: RLProxy listS)

class ShowStorage (listS :: RowList) (rowS :: # Type) (c :: Type -> Type) a
    | listS -> c a, listS -> rowS
  where
    showStorageImpl :: RLProxy listS -> Record rowS -> String

instance showStorageNil :: ShowStorage Nil () c a where
  showStorageImpl _ _ = ""

instance showStorageCons ::
  ( IsSymbol name
  , Storage c a
  , Show (c a)
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , ShowStorage listS' rowS' d b
  ) => ShowStorage (Cons name (c a) listS') rowS c a where
      showStorageImpl _ srec = show (R.get nameP srec) <> "\n" <> rest
        where
          nameP = SProxy :: SProxy name
          rest = showStorageImpl (RLProxy :: RLProxy listS') delrec
          delrec = (R.delete nameP srec) :: Record rowS'

instance compStorageShow ::
  (ShowStorage listS rowS c a
  , RowToList rowS listS) => Show (CompStorage rowS) where
  show (CompStorage srec)= showStorageImpl (RLProxy :: RLProxy listS) srec

class AllocateStorageUniform (listD :: RowList)  (rowS :: # Type) (c :: Type -> Type) a
    | listD c -> rowS a, rowS -> c
  where
    allocateStorageUniformImpl :: RLProxy listD -> Proxy2 c -> Record rowS

instance allocateStorageUniformNil :: AllocateStorageUniform Nil () c a where
  allocateStorageUniformImpl _ _ = {}

instance allocateStorageUniformCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , AllocateStorageUniform listD' rowS' c b
  ) => AllocateStorageUniform (Cons name a listD') rowS c a where
  allocateStorageUniformImpl _ _ = R.insert nameP allocate rest
    where
      nameP = SProxy :: SProxy name
      rest = allocateStorageUniformImpl (RLProxy :: RLProxy listD') (Proxy2 :: Proxy2 c) :: Record rowS'

allocateStorageUniform :: forall c rowD listD a rowS
  . RowToList rowD listD
  => AllocateStorageUniform listD rowS c a
  => Storage c a
  => RProxy rowD
  -> Proxy2 c
  -> CompStorage rowS
allocateStorageUniform _ cprox = CompStorage $ allocateStorageUniformImpl (RLProxy :: RLProxy listD)  cprox


class ReadStorage (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type)  a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    readStorageImpl :: RLProxy listD -> Record rowS -> Int -> Record rowD

instance readStorageNil :: ReadStorage rowS Nil () c a where
   readStorageImpl _ _ _ = {}

instance readStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name a rowD' rowD
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowD'
  , ReadStorage rowS listD' rowD' d b
  ) => ReadStorage rowS (Cons name a listD') rowD c a
    where
      readStorageImpl _ srec ind = R.insert nameP val rest
        where
          nameP = SProxy :: SProxy name
          val = unsafeRead srec nameP ind :: a
          rest = readStorageImpl (RLProxy :: RLProxy listD') srec ind

readStorage :: forall c rowD rowS listD a
  . RowToList rowD listD
  => ReadStorage rowS listD rowD c a
  => CompStorage rowS
  -> Int
  -> Record rowD
readStorage (CompStorage srec) ind = readStorageImpl (RLProxy :: RLProxy listD) srec ind

class MergeStorage   (listS :: RowList) (rowS :: # Type) (c :: Type -> Type) a
    | listS -> rowS, rowS -> a, listS -> c
  where
    mergeStorageImpl :: RLProxy listS -> Record rowS -> Record rowS -> Record rowS

instance mergeStorageNil :: MergeStorage Nil () c a where
  mergeStorageImpl _ _ _ = {}

instance mergeStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , MergeStorage listS' rowS' d b
  ) => MergeStorage (Cons name (c a) listS') rowS c a
    where
      mergeStorageImpl _ upd inp = R.insert nameP val rest
        where
          nameP = SProxy :: SProxy name
          val = merge valUpd valInp
          valUpd = (R.get nameP upd)
          valInp = (R.get nameP inp)
          delUpd = (R.delete nameP upd) :: Record rowS'
          delInp = R.delete nameP inp
          rest = mergeStorageImpl (RLProxy :: RLProxy listS') delUpd delInp

mergeStorage :: forall rowS listS c a
  . RowToList rowS listS
  => MergeStorage listS rowS c a
  => CompStorage rowS
  -> CompStorage rowS
  -> CompStorage rowS
mergeStorage (CompStorage recUpd) (CompStorage recInp) = CompStorage $ mergeStorageImpl rlp recUpd recInp
  where rlp = RLProxy :: RLProxy listS

class WriteStorage (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type) a
    | listD -> rowD, rowD -> a, listD -> c a
  where
    writeStorageImpl :: RLProxy listD -> Record rowS -> Int -> Record rowD -> Record rowS

instance writeStorageNil :: WriteStorage rowS Nil () c a where
    writeStorageImpl _ srec _ _ = srec


instance writeStorageCons ::
  ( IsSymbol name
  , Storage c a
  , RowCons name a rowD' rowD
  , RowLacks name rowD'
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  , WriteStorage rowS listD' rowD' d b
  ) => WriteStorage rowS (Cons name a listD') rowD c a
    where
      writeStorageImpl _ srec ind drec = R.set nameP nstr rest
        where
          nameP = SProxy :: SProxy name
          str = (R.get nameP srec) :: c a
          wrdat = R.get nameP drec
          nstr = set str ind wrdat
          delrec = (R.delete nameP drec) :: Record rowD'
          rest = writeStorageImpl (RLProxy :: RLProxy listD') srec ind delrec

writeStorage :: forall c rowD rowS listD a
  . RowToList rowD listD
  => WriteStorage rowS listD rowD c a
  => CompStorage rowS
  -> Int
  -> Record rowD
  -> CompStorage rowS
writeStorage (CompStorage srec) ind drec = CompStorage $ writeStorageImpl (RLProxy :: RLProxy listD) srec ind drec

class MinIndices (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type) a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    minIndicesImpl :: RLProxy listD -> Record rowS -> Array Int

instance minIndicesBase ::
  ( Storage c a
  , IsSymbol name
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  ) => MinIndices rowS (Cons name a Nil) rowD c a where
  minIndicesImpl _ srec = indices (R.get (SProxy :: SProxy name) srec)

instance minIndicesRec::
  ( Storage c a
  , IsSymbol name
  , RowCons name a rowD' rowD
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowD'
  , MinIndices rowS listD' rowD' d b
  ) => MinIndices rowS (Cons name a listD') rowD c a
    where
      minIndicesImpl _ srec = t
        where
          v =  (indices $ R.get (SProxy :: SProxy name) srec) :: Array Int
          rest = minIndicesImpl (RLProxy :: RLProxy listD') srec
          t = case compare (A.length v) (A.length rest) of
                LT -> v
                GT -> rest
                EQ -> v

minIndices :: forall c rowD rowS listD a
  . RowToList rowD listD
  => MinIndices rowS listD rowD c a
  => CompStorage rowS
  -> RProxy rowD
  -> Array Int
minIndices (CompStorage srec) _ = minIndicesImpl (RLProxy :: RLProxy listD) srec

class IntersectIndices (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type) a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    intersectIndicesImpl :: RLProxy listD -> Record rowS -> Array Int

instance intersectIndicesBase ::
  ( Storage c a
  , IsSymbol name
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  ) => IntersectIndices rowS (Cons name a Nil) rowD c a where
  intersectIndicesImpl _ srec = indices (R.get (SProxy :: SProxy name) srec)

instance intersectIndicesRec ::
  ( Storage c a
  , IsSymbol name
  , RowCons name a rowD' rowD
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowD'
  , IntersectIndices rowS listD' rowD' d b
  ) => IntersectIndices rowS (Cons name a listD') rowD c a
    where
      intersectIndicesImpl _ srec = A.filter f rest
        where
          f x = member (R.get nameP srec) x
          nameP = SProxy :: SProxy name
          rest = intersectIndicesImpl (RLProxy :: RLProxy listD') srec

intersectIndices :: forall c rowD rowS listD a
  . RowToList rowD listD
  => IntersectIndices rowS listD rowD c a
  => CompStorage rowS
  -> RProxy rowD
  -> Array Int
intersectIndices (CompStorage srec) _ = intersectIndicesImpl (RLProxy :: RLProxy listD) srec


applyFn ::  forall rowS rowD rowO listD c a
  . RowToList rowD listD
  => ReadStorage rowS listD rowD c a
  => Storage c a
  => CompStorage rowS -> (Record rowD -> Record rowO) -> Int -> Record rowO
applyFn cs f ind = f sel
  where
    sel = readStorage cs ind :: Record rowD


mapFn :: forall rowS rowD rowO listD m1 a1 m2 a2 m3 a3 m4 a4 listO listS
  . RowToList rowD listD
  => RowToList rowO listO
  => RowToList rowS listS
  => AllocateStorage listS rowS m1 a1
  => ReadStorage rowS listD rowD m2 a2
  => WriteStorage rowS listO rowO m3 a3
  => MergeStorage listS rowS m1 a1
  => Storage m1 a1 => Storage m2 a2 => Storage m3 a3 => Storage m4 a4
  => IntersectIndices rowS listD rowD m4 a4
  => CompStorage rowS -> (Record rowD -> Record rowO) -> CompStorage rowS
mapFn cs f = mergeStorage (foldl fn empt indices) cs
  where
    empt = allocateStorage (RProxy :: RProxy rowS)
    fn m x = writeStorage m x $ applyFn cs f x
    indices = intersectIndices cs (RProxy :: RProxy rowD)
