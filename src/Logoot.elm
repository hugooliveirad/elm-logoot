module Logoot
    exposing
        ( Logoot
        , Pid
        , Positions
        , Position
        , Line
        , Site
        , Clock
        , empty
        , insert
        , remove
        , insertAfter
        , insertAt
        , toDict
        , fromDict
        , diffDict
        , intersectDict
        , keys
        , values
        , toList
        , fromList
        , intersectList
        , diffList
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

@docs Logoot, Pid, Positions, Position, Line, Site, Clock

## Build

@docs empty, insert, remove, insertAfter, insertAt

## Query

future docs isEmpty, member, get, size

## Dictionaries

@docs toDict, fromDict, diffDict, intersectDict

## Lists

It is not recommended to change a `Logoot a` by using those functions.

When you transform a `Logoot a` into a `List (Pid, a)` and back, it loses the
commutative context. This may make your `Logoot a` out-of-sync with other replicas.

Use those functions only at the boundaries of your app, to transform, display, and create.

@docs keys, values, toList, fromList, diffList, intersectList
-}

import Dict as Dict exposing (..)
import Set as Set exposing (..)
import List as List exposing (..)
import Array as Array
import String as String
import Maybe as Maybe exposing (andThen)


-- Types


{-| A Logoot.

`Logoot` implementation details are hidden from the public API.

You should use the provided functions to create and transform a `Logoot a`.
-}
type Logoot a
    = Logoot
        a
        { cemetery : Dict Pid Int
        , content :
            { first : ( Pid, a )
            , intermediate : Dict Pid a
            , last : ( Pid, a )
            }
        , sorted : List ( Pid, a )
        }


{-| -}
type alias Pid =
    ( Positions, Clock )


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


{-| Return an empty `Logoot a`.

An empty `Logoot a` come with the first and last `Pid` in place. They can not be modified.

You should provide an empty representation of your data type. This is used as the content
for the first and last `Pid`

To create a `Logoot String` you could use:

    toDict empty "" == Dict.fromList
      [ (([(0,0)],0), "")
      , (([(32000,0)],0), "")
      ]
-}
empty : a -> Logoot a
empty v =
    Logoot v
        { cemetery = Dict.empty
        , content =
            { first = ( firstPid, v )
            , intermediate = Dict.empty
            , last = ( lastPid, v )
            }
        , sorted = [ ( firstPid, v ), ( lastPid, v ) ]
        }


{-| Insert a key in a `Logoot a`.

This works like `Dict.insert` but with a `Logoot a`.

    empty "" |> insert ([(1, 3)], 15) "it works"

Unlike `Dict.insert`, `insert` is commutative with `remove`,
making it possible to insert and remove keys in any order and
end up with the same `Logoot a`.
-}
insert : Pid -> a -> Logoot a -> Logoot a
insert pid pidcontent ((Logoot t doc) as logoot) =
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
                        Logoot t
                            { doc
                                | content = { content | intermediate = newInter }
                            }
                            |> sortLogoot
                in
                    setDegree pid d dg


{-| Remove a key in a `Logoot a`.

This works like `Dict.remove` but with a `Logoot a`.

    empty ""
      |> insert ([(1, 3)], 15) "it works"
      |> remove ([(1, 3)], 15) "it works"

When you remove a key that isn't a member of `Logoot a`, it will make sure
a future `insert` of that key will not add it to the `Logoot a`.

Unlike `Dict.remove`, `remove` is commutative with `insert`,
making it possible to insert and remove keys in any order and
end up with the same `Logoot a`.
-}
remove : Pid -> a -> Logoot a -> Logoot a
remove pid pidcontent ((Logoot t doc) as logoot) =
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
                Logoot t { doc | content = { content | intermediate = intermediate |> Dict.remove pid } } |> sortLogoot
            else
                setDegree pid logoot (degree pid logoot - 1) |> sortLogoot


{-| Insert a content that will come after `Pid` when `Logoot a` is sorted.
-}
insertAfter : Site -> Clock -> Pid -> a -> Logoot a -> Logoot a
insertAfter site clock pid content logoot =
    let
        leftRight =
            findLeftRight pid logoot

        newPid =
            case leftRight of
                ( Just ( posl, _ ), Just ( posr, _ ) ) ->
                    Just ( posBetween site posl posr, clock )

                _ ->
                    Nothing
    in
        case newPid of
            Just p ->
                insert p content logoot

            Nothing ->
                logoot


{-| Insert a content that will come after a given index.

If the given index is invalid, return Nothing. If valid, Just Logoot
-}
insertAt : Site -> Clock -> Int -> a -> Logoot a -> Maybe (Logoot a)
insertAt site clock index content logoot =
    let
        array =
            logoot
                |> toList
                |> Array.fromList

        left =
            Array.get index array `andThen` (fst >> Just) `andThen` (fst >> Just)

        right =
            Array.get (index + 1) array `andThen` (fst >> Just) `andThen` (fst >> Just)

        pid =
            case ( left, right ) of
                ( Just l, Just r ) ->
                    Just (posBetween site l r)

                _ ->
                    Nothing
    in
        case pid of
            Just p ->
                Just <| insert ( p, clock ) content logoot

            Nothing ->
                Nothing


posBetween : Site -> Positions -> Positions -> Positions
posBetween site posl posr =
    let
        ( poslp, posrp ) =
            padPositions posl posr

        pos =
            zip poslp posrp
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
            if (pos == poslp) || (pos == posrp) then
                pos ++ [ ( 0, site ) ]
            else
                pos
    in
        newPos



-- Query


{-| Determine if a key is in a `Logoot a`. Works as `Dict.member`.
-}
member : Pid -> Logoot a -> Bool
member =
    Dict.member <<. toDict


{-| Get the value associated with a key. Works as `Dict.get`.
-}
get : Pid -> Logoot a -> Maybe a
get =
    Dict.get <<. toDict


{-| Determine the number of key-value pairs in the `Logoot a`. Works as `Dict.size`.
-}
size : Logoot a -> Int
size =
    Dict.size << toDict



-- Dictionaries


{-| Convert a `Logoot a` into a `Dict Pid a` for easier usage.
-}
toDict : Logoot a -> Dict Pid a
toDict =
    Dict.fromList << toList


{-| Convert a `Dict Pid a` into a `Logoot a`.
-}
fromDict : a -> Dict Pid a -> Logoot a
fromDict v =
    fromList v << Dict.toList


{-| Returns a `Dict Pid a` of the pairs that does
   not appear in the second `Logoot a`.
-}
diffDict : Logoot a -> Logoot a -> Dict Pid a
diffDict =
    Dict.diff `on` toDict


{-| Returns `Dict Pid a` of the pairs that appears
   in the second `Logoot a`, preference is given to values in the first `Logoot a`.
-}
intersectDict : Logoot a -> Logoot a -> Dict Pid a
intersectDict =
    Dict.intersect `on` toDict



-- Lists


{-| Get all of the keys in a `Logoot a`, sorted from lowest to highest.
-}
keys : Logoot a -> List Pid
keys =
    List.map fst << toList


{-| Get all of the values in a `Logoot a`, in the order of their keys.
-}
values : Logoot a -> List a
values =
    List.map snd << toList


{-| Convert a `Logoot a` into a sorted association list `List (Pid, a)`.
-}
toList : Logoot a -> List ( Pid, a )
toList (Logoot _ { sorted }) =
    sorted


{-| Convert an association list `List (Pid, a)` into a `Logoot a`.
-}
fromList : a -> List ( Pid, a ) -> Logoot a
fromList v =
    empty v |> List.foldl (uncurry insert)


{-| Returns an association list `List (Pid, a)` of the pairs that does
   not appear in the second `Logoot a`.
-}
diffList : Logoot a -> Logoot a -> List ( Pid, a )
diffList =
    Dict.toList <<< diffDict


{-| Returns an association list `List (Pid, a)` of the pairs that appears
   in the second `Logoot a`, preference is given to values in the first `Logoot a`.
-}
intersectList : Logoot a -> Logoot a -> List ( Pid, a )
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


degree : Pid -> Logoot a -> Int
degree pid (Logoot _ { cemetery }) =
    Dict.get pid cemetery |> Maybe.withDefault 0


setDegree : Pid -> Logoot a -> Int -> Logoot a
setDegree pid (Logoot t doc) d =
    Logoot t <|
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


findLeftRight : Pid -> Logoot a -> ( Maybe Pid, Maybe Pid )
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


sortLogoot : Logoot a -> Logoot a
sortLogoot (Logoot t doc) =
    Logoot t
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
