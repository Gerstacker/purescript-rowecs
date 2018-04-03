module ECS where

import Data.Array (filter)
import Data.Array as A
import Data.Foldable (foldl)
import Data.IntMap as IM
import Data.Maybe (Maybe, fromJust)
import Data.Ord (compare)
import Data.Ordering (Ordering(..))
import Data.Record (insert, get, set, delete) as R
import Partial.Unsafe (unsafePartial)
import Prelude (($), (<>), show, class Show)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), RProxy(RProxy), SProxy(SProxy), reflectSymbol)
import Type.Proxy (Proxy(Proxy), Proxy2(Proxy2))
import Type.Row (Cons, Nil, kind RowList)

class Storage (c :: Type -> Type) a where
  allocate :: c a

class (Storage c a) <= StorageR (c :: Type -> Type) a where
  get :: c a -> Int -> Maybe a
  indices :: c a -> Array Int
  size :: c a -> Int
  member :: c a -> Int -> Boolean

class (Storage c a) <= StorageW (c :: Type -> Type) a where
  set :: c a -> Int -> a -> c a
  del :: c a -> Int -> c a
  merge :: c a -> c a -> c a

class (StorageR c a, StorageW c a) <= StorageRW (c :: Type -> Type) a

instance storageIntMap :: Storage IM.IntMap a where
  allocate = IM.empty

instance storageRIntMap :: StorageR IM.IntMap a where
  get im ind = IM.lookup ind im
  indices im = IM.indices im
  size im = IM.size im
  member im ind = IM.member ind im

instance storageWIntMap :: StorageW IM.IntMap a where
  set im ind val = IM.insert ind val im
  del im ind = IM.delete ind im
  merge = IM.unionRight

instance storageRWIntMap :: StorageRW IM.IntMap a

newtype CompStorage (rowS  :: # Type) = CompStorage (Record rowS)

-- Given a Record of Component containers, select one Component (via SProxy)
-- and return the value at a given index
read :: forall rowS name a c rowS'
  . StorageR c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => Record rowS -> SProxy name -> Int -> Maybe a
read csrec spr ind = get v ind
  where
    v = (R.get spr csrec) :: c a

-- Like read, but assume that value is present
unsafeRead :: forall rowS name a c rowS'
  . IsSymbol name
  => RowCons name (c a) rowS' rowS
  => StorageR c a
  => Record rowS -> SProxy name -> Int -> a
unsafeRead csrec spr ind = unsafePartial $ fromJust $ get v ind
  where
    v = (R.get spr csrec) :: c a

-- Take a Record of Component containers and return a new one with
-- a particular Component updated/inserted at given index
write :: forall rowS name a c rowS'
  . StorageW c a
  => IsSymbol name
  => RowCons name (c a) rowS' rowS
  => Record rowS -> SProxy name -> Int -> a -> Record rowS
write csrec spr ind val = stor'
  where
    intmap = (R.get spr csrec) :: c a
    intmap' = set intmap ind val
    stor' = R.set spr intmap' csrec

-- Recursive type class for traversing a RowList of Component names and types,
-- allocating a Record of Component containers of corresponding contained types
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
  => CompStorage rowS
allocateStorage = CompStorage $ allocateStorageImpl (RLProxy :: RLProxy listS)

-- Recursive type class implementation of Show for Record of containers
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
      showStorageImpl _ srec = show (reflectSymbol nameP) <> " : "<> show (R.get nameP srec) <> "\n" <> rest
        where
          nameP = SProxy :: SProxy name
          rest = showStorageImpl (RLProxy :: RLProxy listS') delrec
          delrec = (R.delete nameP srec) :: Record rowS'

-- Show instance for Record of Component containers
instance compStorageShow ::
  (ShowStorage listS rowS c a
  , RowToList rowS listS) => Show (CompStorage rowS) where
  show (CompStorage srec)= showStorageImpl (RLProxy :: RLProxy listS) srec

-- Like AllocateStorage, but assuming that same container type is used for all
-- Component fields; still exists for largely sentimental reasons
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

-- Recursive TC:
-- Given a CompStorage with many fields and a RowList of a subset of those
-- fields, return a Record containing only the values of the specified fields
-- at a given index. Containers of other unread fields are not traversed at all.
class ReadStorage (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type)  a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    readStorageImpl :: RLProxy listD -> Record rowS -> Int -> Record rowD

instance readStorageNil :: ReadStorage rowS Nil () c a where
   readStorageImpl _ _ _ = {}

instance readStorageCons ::
  ( IsSymbol name
  , StorageR c a
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

-- For applying a recently computed update (modifications or insertions) to an
-- existing CompStorage
class MergeStorage   (listS :: RowList) (rowS :: # Type) (c :: Type -> Type) a
    | listS -> rowS, rowS -> a, listS -> c
  where
    mergeStorageImpl :: RLProxy listS -> Record rowS -> Record rowS -> Record rowS

instance mergeStorageNil :: MergeStorage Nil () c a where
  mergeStorageImpl _ _ _ = {}

instance mergeStorageCons ::
  ( IsSymbol name
  , StorageW c a
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

-- Take an index and a record of fields to write, update the specified slots
-- in the containers in the specified fields. Unwritten containers are not
-- traversed at all, just copied to output CompStorage/Record
class WriteStorage (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type) a
    | listD -> rowD, rowD -> a, listD -> c a
  where
    writeStorageImpl :: RLProxy listD -> Record rowS -> Int -> Record rowD -> Record rowS

instance writeStorageNil :: WriteStorage rowS Nil () c a where
    writeStorageImpl _ srec _ _ = srec


instance writeStorageCons ::
  ( IsSymbol name
  , StorageW c a
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


-- The fields of a CompStorage can have different numbers of elements, identify
-- the smallest and return its array of indices
class MinIndices (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type) a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    minIndicesImpl :: RLProxy listD -> Record rowS -> Array Int

instance minIndicesBase ::
  ( StorageR c a
  , IsSymbol name
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowS'
  ) => MinIndices rowS (Cons name a Nil) rowD c a where
  minIndicesImpl _ srec = indices (R.get (SProxy :: SProxy name) srec)

instance minIndicesRec::
  ( StorageR c a
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

-- Use minIndices to get a least upper bound on intersection of index sets
-- across containers, then intersect it with the index sets of specified
-- Components
-- Component containers in fields not specifed by rowD are not touched
class IntersectIndices (rowS :: # Type) (listD :: RowList) (rowD :: # Type) (c :: Type -> Type) a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    intersectIndicesImpl :: RLProxy listD -> Record rowS -> Array Int -> Array Int

instance intersectIndicesNil :: IntersectIndices rowS Nil () c a where
  intersectIndicesImpl _ _ iw = iw

instance intersectIndicesRec ::
  ( StorageR c a
  , IsSymbol name
  , RowCons name a rowD' rowD
  , RowCons name (c a) rowS' rowS
  , RowLacks name rowD'
  , IntersectIndices rowS listD' rowD' d b
  ) => IntersectIndices rowS (Cons name a listD') rowD c a
    where
      intersectIndicesImpl _ srec iw = A.filter f rest
        where
          f x = member (R.get nameP srec) x
          nameP = SProxy :: SProxy name
          rest = intersectIndicesImpl (RLProxy :: RLProxy listD') srec iw

intersectIndices :: forall c rowD rowS listD a
  . RowToList rowD listD
  => IntersectIndices rowS listD rowD c a
  => MinIndices rowS listD rowD c a
  => CompStorage rowS
  -> RProxy rowD
  -> Array Int
intersectIndices cs@(CompStorage srec) rp = intersectIndicesImpl rlp srec minind
  where
    rlp = RLProxy :: RLProxy listD
    minind = minIndices cs rp

-- Given a CompStorage and a function from one subset of Components to another
-- apply the function at a given index (for one entity) and return the new
-- values of those fields in a new Record with only those fields
applyFn ::  forall rowS rowD rowO listD c a
  . RowToList rowD listD
  => ReadStorage rowS listD rowD c a
  => Storage c a
  => CompStorage rowS -> (Record rowD -> Record rowO) -> Int -> Record rowO
applyFn cs f ind = f sel
  where
    sel = readStorage cs ind :: Record rowD

applyFnScalar :: forall rowS rowD t listD c a
  . RowToList rowD listD
  => ReadStorage rowS listD rowD c a
  => StorageR c a
  => CompStorage rowS -> (Record rowD -> t) -> Int -> t
applyFnScalar cs f ind = (fr sel).x
  where
    sel = readStorage cs ind :: Record rowD
    fr :: Record rowD -> Record ( x :: t )
    fr g = { x : f g }


-- Take a CompStorage and a function from one subset of fields to another
-- subset. Find (using intersectIndices) the set of all indices for which
-- all input fields are present, map the function over that set, return
-- a new CompStorage with those fields updated. Containers that are neither
-- input nor output are simply copied to output CompStorage
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
  => MinIndices rowS listD rowD m4 a4
  => CompStorage rowS -> (Record rowD -> Record rowO) -> CompStorage rowS
mapFn cs f = mergeStorage (foldl fn empt indices) cs
  where
    empt = allocateStorage
    fn m x = writeStorage m x $ applyFn cs f x
    indices = intersectIndices cs (RProxy :: RProxy rowD)

class DropPred (rowS :: # Type) ( listD :: RowList) (rowD :: # Type) (c :: Type -> Type) a
    | listD -> rowD, rowD -> a, listD rowS -> c
  where
    dropPredImpl :: RLProxy listD -> Record rowS -> Array Int -> Record rowS

instance dropPredNil :: DropPred rowS Nil () c a where
  dropPredImpl _ srec _ = srec

instance dropPredRec ::
   ( StorageRW c a
   , IsSymbol name
   , RowCons name a rowD' rowD
   , RowCons name (c a) rowS' rowS
   , DropPred rowS listD' rowD' d b
   ) => DropPred rowS (Cons name a listD') rowD c a
     where
       dropPredImpl _ srec ind = R.set nameP nstr rest
         where
           nameP = SProxy :: SProxy name
           str = (R.get nameP srec) :: c a
           nstr = foldl fn str ind
           fn :: c a -> Int -> c a
           fn = del
           rest = dropPredImpl (RLProxy :: RLProxy listD') srec ind


dropPred :: forall rowS rowD listD m1 a1 m2 a2 m3 a3
  . RowToList rowD listD
  => IntersectIndices rowS listD rowD m1 a1
  => MinIndices rowS listD rowD m1 a1
  => ReadStorage rowS listD rowD m3 a3
  => StorageRW m1 a1 => StorageR m3 a3
  => DropPred rowS listD rowD m1 a1
  => CompStorage rowS -> (Record rowD -> Boolean) -> CompStorage rowS
dropPred cs@(CompStorage srec) p =  CompStorage $ dropPredImpl (RLProxy :: RLProxy listD) srec flt
    where
      ind = intersectIndices cs (RProxy :: RProxy rowD)
      flt = filter (applyFnScalar cs p) ind
