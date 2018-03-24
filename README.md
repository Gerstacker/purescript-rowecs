# purescript-rowecs

A Purescript implementation of [Entity-Component-Systems](https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system) that leans heavily on Row types and nasty recursive type classes to iterate over their fields. API aims to be elegant and rigorously type safe.

Currently only IntMap is supported as the container for each Component type. Original repository (which is itself a port from the Haskell library) is [purescript-intmaps](https://github.com/tel/purescript-intmaps), my fork updated to build under 0.11 is [here](https://github.com/Gerstacker/purescript-intmaps).

Instances for other container types (e.g. hash-based) should be easy to implement.

## Basic Ideas

The top-level CompStorage type is a newtype wrapper around a Record type.

~~~
newtype CompStorage (rowS  :: # Type) = CompStorage (Record rowS)
~~~

rowS is a Row type specifying the names and types of containers of all the Components that an entity might possibly have in this universe.

~~~
type SRow = (a::IntMap Int, b::IntMap String, c::IntMap Number)
type CS = CompStorage SRow
~~~

Here SRow says that an entity might have a Component "a", which has to be an Int, a b::String, and/or a c::Number. Any entity may have any (probably non-empty) subset of those Components.

Given a storage Row type like SRow, we can allocate an empty CompStorage with allocateStorage:

~~~
het0 :: CS
het0 = allocateStorage
~~~

het0 is a CompStorage, which is a Record of containers of Components.

If we want to create an entity with index 55 whose only attribute is a value of 2.3 for its "c" field, it's pretty easy:

~~~
het1 :: CS
het1 = writeStorage het0 55 { c:2.3 }
~~~

Inside writeStorage is the (type-level, compile-time) logic to look at the fields of the Record you've provided to write at index 55 and make the appropriate update to only the modified container(s), returning a new Record of containers. Container fields not specified in the Record argument to writeStorage are not touched, just copied to the output CompStorage.


## Mapping "Systems" over a CompStorage

Imagine you have a CompStorage representing a number of Entities, each of which has some subset of the Components known to the CompStorage. For example there are mobile Entities that have both Position and Velocity and stationary Entities that have only Position. You'd like to apply kinematic updates to only the mobile ones without having to bother with the stationary. Implement a function of the right type:

~~~
kinematics :: Record {position::Vec3, velocity::Vec3, acceleration::Vec3} -> Record {position::Vec3, velocity::Vec3}
~~~

and hand it to mapFn:

~~~
newCS = mapFn oldCS kinematics
~~~

and you have a new CompStorage with the Positions updated for only those entities that had both a Position and a Velocity in oldCS. Other entities (e.g. stationary buildings that have no Velocity) are simply copied from old to new. Components not involved in the computation have their containers simply copied from old to new. If you make a type error like getting the type of a field wrong the compiler will let you know, though usually quite opaquely.

## Future directions

### Composition of Systems
It seems like it should be straightforward to implement composition of two Systems into a single new System. For example, kinematics and bounceOffWalls may take pretty much the same inputs and modify the same Components and would most efficiently be run in one call to mapFn.

### Mutability/ST support
Mutable-friendly containers (maybe hash-based) may be more efficient in many situations. One time step would consist of an ST session starting with a frozen/pure CompStorage from the last time step and running a number of mapFn's on a mutable working copy to be frozen and used as input to the next time step.

### Can FRP and ECS co-exist?
At first glance they both seem to demand control over the main loop, but maybe there's a hybrid that makes some sense. Consider a CompStorage field that behaves like a memory-mapped register of a hardware device. Writes to this field don't store component data per se, but generate events to be consumed elsewhere. Likewise, a field may store queues of pending events destined for each of a set of entities. mapFn with an appropriate System would straightforwardly select and process those entities that have pending events.
I'm experimenting with something like this to manage deleting entities. mapFn is called with a predicate (like "entity leaves playing field") that may mark the entity as to-be-dropped.
