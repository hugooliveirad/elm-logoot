module Logoot exposing (
  empty, insert, remove, toDict, initialPid, lastPid, comparePid, comparePos, posBetween, padPositions,
  Doc, Cemetery, Content, Pid, PidContent, Positions, Position, Line, Site, Clock)

{-|
  Simple Logoot implementation

## Parts API

@docs empty, insert, remove, toDict, initialPid, lastPid, comparePid, comparePos, posBetween, padPositions

## Types

@docs Doc, Cemetery, Content, Pid, PidContent, Positions, Position, Line, Site, Clock
-}

import Dict as Dict exposing (..)
import Set as Set exposing (..)
import List as List exposing (..)
import Array as Array
import String as String

{-| -}
type alias Doc =
  { cemetery : Cemetery
  , content : Content
  }
{-| -}
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

maxInt = 32000

{-| First Pid in every Logoot Doc -}
initialPid : Pid
initialPid = ([(0,0)], 0)

{-| Last Pid in every Logoot Doc -}
lastPid : Pid
lastPid = ([(maxInt,0)], 0)

{-| Empty Logoot.
-}
empty : Doc
empty =
  { cemetery = Dict.empty
  , content = Dict.fromList [ (initialPid, ""), (lastPid, "") ]
  }

(<<<) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(<<<) f g a b = f (g a b)

(<<.) : (a -> b -> c) -> (d -> b) -> a -> d -> c
(<<.) f g a b = f a (g b)

degree : Pid -> Doc -> Int
degree = Maybe.withDefault 0 <<< get <<. .cemetery

setDegree : Pid -> Doc -> Int -> Doc
setDegree pid doc d =
  if d == 0
  then {doc | cemetery = Dict.remove pid doc.cemetery}
  else {doc | cemetery = Dict.insert pid d doc.cemetery}

{-| Insert a key in a Doc 
-}
insert : Pid -> PidContent -> Doc -> Doc
insert pid content doc =
  let
    dg = degree pid doc + 1
    d = {doc | content = Dict.insert pid content doc.content}
  in
    if dg == 0
    then doc
    else setDegree pid d dg

{-| -}
remove : Pid -> PidContent -> Doc -> Doc
remove pid content doc =
  if Dict.member pid doc.content
  then { doc | content = Dict.remove pid doc.content}
  else setDegree pid doc (degree pid doc - 1)

{-| Compare two Postionstion
-} 
comparePos : Positions -> Positions -> Order
comparePos posl posr =
  let
    compAll = (List.filter ((/=) EQ) >> take 1) (map2 compare posl posr)
  in
    case compAll of
      [] -> compare (length posl) (length posr)
      res -> Maybe.withDefault EQ (head res)

{-| Compares two Pids
-}
comparePid : Pid -> Pid -> Order
comparePid pidl pidr =
  case comparePos (fst pidl) (fst pidr) of
    EQ -> compare (snd pidl) (snd pidr)
    x -> x

zip : List a -> List b -> List (a,b)
zip = map2 (,)

find : (a -> Bool) -> List a -> Maybe a
find pred = List.filter pred >> head

{-| padding
-}
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

{-| Generate a Pos between two Pos.
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

sortPids : List Pid -> List Pid
sortPids pids =
  sortWith comparePid pids

findLeftRight : Pid -> Doc -> (Maybe Pid, Maybe Pid)
findLeftRight pid doc =
  let
    pids = doc.content
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

insertAfter : Pid -> Site -> Clock -> PidContent -> Doc -> Doc
insertAfter pid site clock content doc =
  let
    leftRight = findLeftRight pid doc
    newPid = case leftRight of
      (Just (posl, _), Just (posr, _)) -> Just (posBetween site posl posr, clock)
      _ -> Nothing
  in
    case newPid of
      Just p -> insert p content doc
      Nothing -> doc

{-| Transforms a Doc into a Dict for easier usage
-}
toDict : Doc -> Dict Pid PidContent
toDict = .content

toString : Doc -> String
toString doc =
  doc.content
    |> Dict.toList
    |> sortWith (\(l, _) (r, _) -> comparePid l r)
    |> List.map snd
    |> String.join ""

