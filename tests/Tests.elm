module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Dict
import Fuzz as F
import Logoot exposing (..)


type Operation
    = Insert
    | Remove
    | Noop


maxInt : Int
maxInt =
    32000


firstPid : Pid
firstPid =
    ( [ ( 0, 0 ) ], 0 )


lastPid : Pid
lastPid =
    ( [ ( maxInt, 0 ) ], 0 )


on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b =
    f (g a) (g b)


toNonEmptyPositions a =
    case a of
        [] ->
            [ ( 1, 0 ) ]

        _ ->
            a


uniqueOps =
    Dict.values << List.foldl (\( o, p, c ) -> Dict.insert p ( o, p, c )) Dict.empty


natF =
    F.intRange 1 100


lineF =
    natF


siteF =
    natF


clockF =
    natF


pidContentF =
    F.string


positionF =
    F.tuple ( lineF, siteF )


positionsF =
    F.list positionF |> F.map toNonEmptyPositions


pidF =
    F.tuple ( positionsF, clockF )


opF =
    F.frequencyOrCrash [ ( 1, F.constant Insert ), ( 1, F.constant Remove ) ]


opsF =
    F.list (F.tuple3 ( opF, pidF, pidContentF )) |> F.map uniqueOps


applyOps =
    List.foldl applyOpTuple |> flip


uncurry3 f ( a, b, c ) =
    f a b c


applyOpTuple =
    uncurry3 applyOp


applyOp : Operation -> Pid -> a -> Logoot a -> Logoot a
applyOp op =
    case op of
        Insert ->
            insert

        Remove ->
            remove

        Noop ->
            \_ _ d -> d


all : Test
all =
    describe "elm-logoot test suite"
        [ describe "ops properties"
            [ fuzz opsF "idempotent" <|
                \ops ->
                    on Expect.equal
                        Logoot.toList
                        (applyOps ops <| empty "")
                        (applyOps ops <| empty "")
            , fuzz opsF "commutative" <|
                \ops ->
                    on Expect.equal
                        Logoot.toList
                        (applyOps ops <| empty "")
                        (applyOps (List.reverse ops) <| empty "")
            , fuzz opsF "associative" <|
                \ops ->
                    on Expect.equal
                        toList
                        (empty "" |> applyOps (List.reverse ops) |> applyOps ops)
                        (empty "" |> applyOps ops |> applyOps (List.reverse ops))
            ]
        , describe "insertAfter properties"
            [ fuzz pidF "always after" <|
                \pid ->
                    empty ""
                        |> insert pid "hel"
                        |> insertAfter 2 2 pid "lo!"
                        |> toList
                        |> List.map snd
                        |> String.join ""
                        |> Expect.equal "hello!"
            ]
        ]
