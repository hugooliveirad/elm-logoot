module Logoot exposing
  ( Logoot, Pid, PidContent, Positions, Position, Line, Site, Clock
  , empty, insert, remove, insertAfter, posBetween
  , toDict
  , keys, values, toList
  , sortPids, comparePid, comparePos
  )

{-| Simple Logoot implementation

Logoot is a [Conflict-free Replicated Data Type][crdt] (CRDT) created to be used by
distributed systems that want to achieve Strong Eventual Consistency (SEC).

[crdt]: https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type

Insert and remove operations in Logoot are idempotent, commutative and associative.
This means two replicas can exchange operations made in their local Logoot over a
network without having to garantee the order of messages, enabling their use with
distributed networks such as P2P.

This is an implementation of [Logoot-undo][logoot] propose by Stéphane Weiss,
Pascal Urso and Pascal Molli. It still lacks support for undo operations.

[logoot]: https://pdfs.semanticscholar.org/75e4/5cd9cae6d0da1faeae11732e39a4c1c7a17b.pdf

There are a lot of missing pieces here, help us sending PRs to the GitHub [repository]!

[repository]: https://github.com/hugobessaa/elm-logoot

## Types

@docs Logoot, Pid, PidContent, Positions, Position, Line, Site, Clock

## Build

@docs empty, insert, remove, insertAfter, posBetween

## Dictionaries

@docs toDict

## Lists

@docs keys, values, toList

## Sort and compare

@docs sortPids, comparePid, comparePos

-}

import Dict as Dict exposing (..)
import Set as Set exposing (..)
import List as List exposing (..)
import Array as Array
import String as String

-- Types

{-| A Logoot.

`Logoot` implementation details are hidden from the public API.

You should use the provided functions to transform a `Logoot`.
-}
type Logoot =
  Logoot
    { cemetery : Cemetery
    , content : Content
    }
type alias Cemetery = Dict Pid Int
{-| -}
type alias Content = Dict Pid PidContent
{-| -}
type alias Pid = (Positions, Clock)
{-| -}
type alias PidContent = String
{-| -}
type alias Positions = List Position
{-| -}
type alias Position = (Line, Site)
{-| -}
type alias Line = Int
{-| -}
type alias Site = Int
{-| -}
type alias Clock = Int

-- Constants

maxInt : Int
maxInt = 32000

firstPid : Pid
firstPid = ([(0,0)],0)

lastPid : Pid
lastPid = ([(maxInt,0)],0)

-- Build

{-| Return an empty `Logoot`.

An empty `Logoot` come with the first and last `Pid` in place. They should not be removed.

    toDict empty == Dict.fromList
      [ (([(0,0)],0), "")
      , (([(maxInt,0)],0), "") 
      ]
-}
empty : Logoot
empty =
  Logoot
    { cemetery = Dict.empty
    , content = Dict.fromList [ (firstPid, ""), (lastPid, "") ]
    }


{-| Insert a key in a `Logoot`.

This works like `Dict.insert` but with a `Logoot`.

    empty |> insert ([(1, 3)], 15) "it works"

Unlike `Dict.insert`, `insert` is commutative with `remove`,
making it possible to insert and remove keys in any order and
end up with the same `Logoot`.
-}
insert : Pid -> PidContent -> Logoot -> Logoot
insert pid content (Logoot doc) =
  let
    dg = degree pid (Logoot doc) + 1
    d = Logoot {doc | content = Dict.insert pid content doc.content}
  in
    if dg == 0
    then Logoot doc
    else setDegree pid d dg

{-| Remove a key in a `Logoot`.

This works like `Dict.remove` but with a `Logoot`.

    empty
      |> insert ([(1, 3)], 15) "it works"
      |> remove ([(1, 3)], 15) "it works"

When you remove a key that isn't a member of `Logoot`, it will make sure
a future `insert` of that key will not add it to the `Logoot`.

Unlike `Dict.remove`, `remove` is commutative with `insert`,
making it possible to insert and remove keys in any order and
end up with the same `Logoot`.
-}
remove : Pid -> PidContent -> Logoot -> Logoot
remove pid content (Logoot doc) =
  if Dict.member pid doc.content
  then Logoot { doc | content = Dict.remove pid doc.content}
  else setDegree pid (Logoot doc) (degree pid (Logoot doc) - 1)

{-| Insert `PidContent` that will come after `Pid` when `Logoot` is sorted.
-}
insertAfter : Site -> Clock -> PidContent -> Pid -> Logoot -> Logoot
insertAfter site clock content pid  doc =
  let
    leftRight = findLeftRight pid doc
    newPid = case leftRight of
      (Just (posl, _), Just (posr, _)) -> Just (posBetween site posl posr, clock)
      _ -> Nothing
  in
    case newPid of
      Just p -> insert p content doc
      Nothing -> doc

{-| Generate `Positions` between two `Positions`.
-}
posBetween : Site -> Positions -> Positions -> Positions
posBetween site posl posr =
  let
    pos = (posl, posr) 
      |> uncurry padPositions
      |> uncurry zip
      |> List.foldl (\(p1, p2) (folded, acc) ->
        if folded then (folded, acc)
        else case compare (fst p1) (fst p2) of
          EQ -> (False, acc ++ [p1])
          LT -> if fst p1 + 1 < fst p2
            then (True, acc ++ [(fst p1 + 1, site)])
            else (True, posl ++ [(0, site)])
          GT -> (False, acc ++ [p1])) 
        (False, [])
       |> snd
    newPos = if pos == posl
      then pos ++ [(0, site)]
      else pos
  in
    newPos

-- Query

-- TODO: isEmpty
-- TODO: member
-- TODO: get
-- TODO: size

-- Dictionaries

{-| Convert a `Logoot` into a `Dict` for easier usage.
-}
toDict : Logoot -> Dict Pid PidContent
toDict (Logoot {content}) = content

-- TODO: fromDict

-- Lists

{-| Get all of the keys in a `Logoot`, sorted from lowest to highest.
-}
keys : Logoot -> List Pid
keys = List.map fst << toList

{-| Get all of the values in a `Logoot`, in the order of their keys.
-}
values : Logoot -> List PidContent
values = List.map snd << toList

{-| Convert a `Logoot` into a sorted association list of `Pid`-`PidContent` pairs.
-}
toList : Logoot -> List (Pid, PidContent)
toList = sortWith (comparePid `on` fst) << Dict.toList << toDict

-- TODO: fromList

-- Transform

-- TODO: map
-- TODO: foldl
-- TODO: foldr
-- TODO: filter
-- TODO: partition

-- Combine

-- TODO: union
-- TODO: intersect
-- TODO: diff
-- TODO: merge

-- Sort and Compare

{-| Sort a `List Pid` using `comparePid`. -}
sortPids : List Pid -> List Pid
sortPids pids =
  sortWith comparePid pids

{-| Compare two `Positions`. -}
comparePos : Positions -> Positions -> Order
comparePos posl posr =
  let
    compAll = (List.filter ((/=) EQ) >> take 1) (map2 compare posl posr)
  in
    case compAll of
      [] -> compare (length posl) (length posr)
      res -> Maybe.withDefault EQ (head res)

{-| Compare two `Pid`s -}
comparePid : Pid -> Pid -> Order
comparePid pidl pidr =
  case comparePos (fst pidl) (fst pidr) of
    EQ -> compare (snd pidl) (snd pidr)
    x -> x

-- Private helpers

degree : Pid -> Logoot -> Int
degree pid (Logoot {cemetery}) = get pid cemetery |> Maybe.withDefault 0

setDegree : Pid -> Logoot -> Int -> Logoot
setDegree pid (Logoot doc) d =
  Logoot <| 
    if d == 0
    then {doc | cemetery = Dict.remove pid doc.cemetery}
    else {doc | cemetery = Dict.insert pid d doc.cemetery}

padPositions : Positions -> Positions -> (Positions, Positions)
padPositions p1 p2 =
  let
    l1 = length p1
    l2 = length p2
    diff = abs (l1 - l2)
  in
    case compare l1 l2 of
      EQ -> (p1, p2)
      LT -> (p1 ++ (repeat diff (0,-1)), p2)
      GT -> (p1, p2 ++ (repeat diff (0,-1)))

findLeftRight : Pid -> Logoot -> (Maybe Pid, Maybe Pid)
findLeftRight pid (Logoot {content}) =
  let
    pids = content
      |> Dict.keys
      |> sortPids
      |> Array.fromList
    indexed = pids
      |> Array.toIndexedList
      |> find (\x -> snd x == pid)
    index = case indexed of
       Just (i, _) -> Just i
       Nothing -> Nothing
  in
    case index of
      Just i -> (Array.get i pids, Array.get (i+1) pids)
      Nothing -> (Nothing, Nothing)

-- TODO: Move these helpers out of this file

(<<<) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(<<<) f g a b = f (g a b)

(<<.) : (a -> b -> c) -> (d -> b) -> a -> d -> c
(<<.) f g a b = f a (g b)

zip : List a -> List b -> List (a,b)
zip = map2 (,)

find : (a -> Bool) -> List a -> Maybe a
find pred = List.filter pred >> head

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b = f (g a) (g b)
