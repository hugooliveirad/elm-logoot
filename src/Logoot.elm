module Logoot
    exposing
        ( Logoot
        , Pid
        , PidContent
        , Positions
        , Position
        , Line
        , Site
        , Clock
        , empty
        , insert
        , remove
        , insertAfter
        , posBetween
        , isEmpty
        , member
        , get
        , size
        , toDict
        , fromDict
        , keys
        , values
        , toList
        , fromList
        , diffList
        , intersectList
        )

{-| Simple Logoot implementation.

Insert and remove operations in Logoot are idempotent, commutative and associative.
This means two replicas can exchange operations made in their local Logoot over a
network without having to garantee the order of messages, enabling their use with
distributed networks such as P2P.

Here are various functions to deal with the Logoot document. There are a lot of
missing pieces here, help us sending PRs to the GitHub [repository]!

[repository]: https://github.com/hugobessaa/elm-logoot

## Types

@docs Logoot, Pid, PidContent, Positions, Position, Line, Site, Clock

## Build

@docs empty, insert, remove, insertAfter, posBetween

## Query

@docs isEmpty, member, get, size

## Dictionaries

@docs toDict, fromDict

## Lists

@docs keys, values, toList, fromList, diffList, intersectList
-}

import Dict as Dict exposing (..)
import Set as Set exposing (..)
import List as List exposing (..)
import Array as Array
import String as String


-- Types


{-| A Logoot.

`Logoot` implementation details are hidden from the public API.

You should use the provided functions to create and transform a `Logoot`.
-}
type Logoot
    = Logoot
        { cemetery : Cemetery
        , content : Content
        , sorted : List ( Pid, PidContent )
        }


type alias Cemetery =
    Dict Pid Int


{-| -}
type alias Content =
    { first : ( Pid, PidContent )
    , intermediate : Dict Pid PidContent
    , last : ( Pid, PidContent )
    }


{-| -}
type alias Pid =
    ( Positions, Clock )


{-| -}
type alias PidContent =
    String


{-| -}
type alias Positions =
    List Position


{-| -}
type alias Position =
    ( Line, Site )


{-| -}
type alias Line =
    Int


{-| -}
type alias Site =
    Int


{-| -}
type alias Clock =
    Int



-- Constants


maxInt : Int
maxInt =
    32000


firstPid : Pid
firstPid =
    ( [ ( 0, 0 ) ], 0 )


lastPid : Pid
lastPid =
    ( [ ( maxInt, 0 ) ], 0 )



-- Build


{-| Return an empty `Logoot`.

An empty `Logoot` come with the first and last `Pid` in place. They can not be modified.

    toDict empty == Dict.fromList
      [ (([(0,0)],0), "")
      , (([(32000,0)],0), "")
      ]
-}
empty : Logoot
empty =
    Logoot
        { cemetery = Dict.empty
        , content =
            { first = ( firstPid, "" )
            , intermediate = Dict.empty
            , last = ( lastPid, "" )
            }
        , sorted = [ ( firstPid, "" ), ( lastPid, "" ) ]
        }


{-| Insert a key in a `Logoot`.

This works like `Dict.insert` but with a `Logoot`.

    empty |> insert ([(1, 3)], 15) "it works"

Unlike `Dict.insert`, `insert` is commutative with `remove`,
making it possible to insert and remove keys in any order and
end up with the same `Logoot`.
-}
insert : Pid -> PidContent -> Logoot -> Logoot
insert pid pidcontent ((Logoot doc) as logoot) =
    if (pid == firstPid) || (pid == lastPid) then
        logoot
    else
        let
            dg =
                degree pid logoot + 1
        in
            if dg == 0 then
                logoot
            else
                let
                    content =
                        doc.content

                    intermediate =
                        content.intermediate

                    newInter =
                        intermediate |> Dict.insert pid pidcontent

                    d =
                        Logoot
                            { doc
                                | content = { content | intermediate = newInter }
                            }
                            |> sortLogoot
                in
                    setDegree pid d dg


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
remove pid pidcontent ((Logoot doc) as logoot) =
    if (pid == firstPid) || (pid == lastPid) then
        logoot
    else
        let
            content =
                doc.content

            intermediate =
                content.intermediate
        in
            if toDict logoot |> Dict.member pid then
                Logoot { doc | content = { content | intermediate = intermediate |> Dict.remove pid } } |> sortLogoot
            else
                setDegree pid logoot (degree pid logoot - 1) |> sortLogoot


{-| Insert `PidContent` that will come after `Pid` when `Logoot` is sorted.
-}
insertAfter : Site -> Clock -> Pid -> PidContent -> Logoot -> Logoot
insertAfter site clock pid content doc =
    let
        leftRight =
            findLeftRight pid doc

        newPid =
            case leftRight of
                ( Just ( posl, _ ), Just ( posr, _ ) ) ->
                    Just ( posBetween site posl posr, clock )

                _ ->
                    Nothing
    in
        case newPid of
            Just p ->
                insert p content doc

            Nothing ->
                doc


{-| Generate `Positions` between two `Positions`.
-}
posBetween : Site -> Positions -> Positions -> Positions
posBetween site posl posr =
    let
        pos =
            ( posl, posr )
                |> uncurry padPositions
                |> uncurry zip
                |> List.foldl
                    (\( p1, p2 ) ( folded, acc ) ->
                        if folded then
                            ( folded, acc )
                        else
                            case compare (fst p1) (fst p2) of
                                EQ ->
                                    ( False, acc ++ [ p1 ] )

                                LT ->
                                    if fst p1 + 1 < fst p2 then
                                        ( True, acc ++ [ ( fst p1 + 1, site ) ] )
                                    else
                                        ( True, posl ++ [ ( 0, site ) ] )

                                GT ->
                                    ( False, acc ++ [ p1 ] )
                    )
                    ( False, [] )
                |> snd

        newPos =
            if pos == posl then
                pos ++ [ ( 0, site ) ]
            else
                pos
    in
        newPos



-- Query


{-| Determine if a `Logoot` is empty. Works as `Dict.isEmpty`.
-}
isEmpty : Logoot -> Bool
isEmpty =
    Dict.isEmpty << toDict


{-| Determine if a key is in a `Logoot`. Works as `Dict.member`.
-}
member : Pid -> Logoot -> Bool
member =
    Dict.member <<. toDict


{-| Get the value associated with a key. Works as `Dict.get`.
-}
get : Pid -> Logoot -> Maybe PidContent
get =
    Dict.get <<. toDict


{-| Determine the number of key-value pairs in the `Logoot`. Works as `Dict.size`.
-}
size : Logoot -> Int
size =
    Dict.size << toDict



-- Dictionaries


{-| Convert a `Logoot` into a `Dict Pid PidContent` for easier usage.
-}
toDict : Logoot -> Dict Pid PidContent
toDict =
    Dict.fromList << toList


{-| Convert a `Dict Pid PidContent` into a `Logoot`.
-}
fromDict : Dict Pid PidContent -> Logoot
fromDict =
    fromList << Dict.toList


{-| Returns a `Dict Pid PidContent` of the pairs that does
not appear in the second `Logoot`.
-}
diffDict : Logoot -> Logoot -> Dict Pid PidContent
diffDict =
    Dict.diff `on` toDict


{-| Returns `Dict Pid PidContent` of the pairs that appears
in the second `Logoot`, preference is given to values in the first `Logoot`.
-}
intersectDict : Logoot -> Logoot -> Dict Pid PidContent
intersectDict =
    Dict.intersect `on` toDict



-- Lists


{-| Get all of the keys in a `Logoot`, sorted from lowest to highest.
-}
keys : Logoot -> List Pid
keys =
    List.map fst << toList


{-| Get all of the values in a `Logoot`, in the order of their keys.
-}
values : Logoot -> List PidContent
values =
    List.map snd << toList


{-| Convert a `Logoot` into a sorted association list `List (Pid, PidContent)`.
-}
toList : Logoot -> List ( Pid, PidContent )
toList (Logoot { sorted }) =
    sorted


{-| Convert an association list `List (Pid, PidContent)` into a `Logoot`.
-}
fromList : List ( Pid, PidContent ) -> Logoot
fromList =
    empty |> List.foldl (uncurry insert)


{-| Returns an association list `List (Pid, PidContent)` of the pairs that does
not appear in the second `Logoot`.
-}
diffList : Logoot -> Logoot -> List ( Pid, PidContent )
diffList =
    Dict.toList <<< diffDict


{-| Returns an association list `List (Pid, PidContent)` of the pairs that appears
in the second `Logoot`, preference is given to values in the first `Logoot`.
-}
intersectList : Logoot -> Logoot -> List ( Pid, PidContent )
intersectList =
    Dict.toList <<< intersectDict



-- Transform
-- Does it makes sense to open the `Logoot` this way?
-- TODO: map
-- TODO: foldl
-- TODO: foldr
-- TODO: filter
-- TODO: partition
-- Private helpers


comparePos : Positions -> Positions -> Order
comparePos posl posr =
    let
        compAll =
            (List.filter ((/=) EQ) >> take 1) (map2 compare posl posr)
    in
        case compAll of
            [] ->
                compare (length posl) (length posr)

            res ->
                Maybe.withDefault EQ (head res)


comparePid : Pid -> Pid -> Order
comparePid pidl pidr =
    case comparePos (fst pidl) (fst pidr) of
        EQ ->
            compare (snd pidl) (snd pidr)

        x ->
            x


degree : Pid -> Logoot -> Int
degree pid (Logoot { cemetery }) =
    Dict.get pid cemetery |> Maybe.withDefault 0


setDegree : Pid -> Logoot -> Int -> Logoot
setDegree pid (Logoot doc) d =
    Logoot <|
        if d == 0 then
            { doc | cemetery = Dict.remove pid doc.cemetery }
        else
            { doc | cemetery = Dict.insert pid d doc.cemetery }


padPositions : Positions -> Positions -> ( Positions, Positions )
padPositions p1 p2 =
    let
        l1 =
            length p1

        l2 =
            length p2

        diff =
            abs (l1 - l2)
    in
        case compare l1 l2 of
            EQ ->
                ( p1, p2 )

            LT ->
                ( p1 ++ (repeat diff ( 0, -1 )), p2 )

            GT ->
                ( p1, p2 ++ (repeat diff ( 0, -1 )) )


findLeftRight : Pid -> Logoot -> ( Maybe Pid, Maybe Pid )
findLeftRight pid logoot =
    let
        pids =
            logoot
                |> toList
                |> List.map fst
                |> Array.fromList

        indexed =
            pids
                |> Array.toIndexedList
                |> find (\x -> snd x == pid)

        index =
            case indexed of
                Just ( i, _ ) ->
                    Just i

                Nothing ->
                    Nothing
    in
        case index of
            Just i ->
                ( Array.get i pids, Array.get (i + 1) pids )

            Nothing ->
                ( Nothing, Nothing )


sortLogoot : Logoot -> Logoot
sortLogoot (Logoot doc) =
    Logoot
        { doc
            | sorted = [ doc.content.first ] ++ Dict.toList doc.content.intermediate ++ [ doc.content.last ] |> sortWith (comparePid `on` fst)
        }



-- TODO: Move these helpers out of this file


(<<<) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(<<<) f g a b =
    f (g a b)


(<<.) : (a -> b -> c) -> (d -> b) -> a -> d -> c
(<<.) f g a b =
    f a (g b)


zip : List a -> List b -> List ( a, b )
zip =
    map2 (,)


find : (a -> Bool) -> List a -> Maybe a
find pred =
    List.filter pred >> head


on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b =
    f (g a) (g b)
