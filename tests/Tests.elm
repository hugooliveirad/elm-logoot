module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Dict
import Fuzz as F
import Logoot exposing (..)


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


pids =
    { small = ( [ ( 1, 1 ) ], 0 )
    , smallTwo = ( [ ( 1, 1 ), ( 2, 2 ) ], 0 )
    , medium = ( [ ( 2, 2 ) ], 0 )
    , mediumTwo = ( [ ( 2, 2 ), ( 3, 4 ) ], 0 )
    , mediumMany = ( [ ( 2, 2 ), ( 3, 4 ), ( 5, 6 ), ( 7, 8 ) ], 0 )
    , big = ( [ ( 30, 5 ) ], 0 )
    , bigTwo = ( [ ( 30, 5 ), ( 40, 7 ) ], 0 )
    , bigTwoSmaller = ( [ ( 30, 5 ), ( 35, 6 ) ], 0 )
    }


posBetweenTwo =
    posBetween 2


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
    F.frequencyOrCrash [ ( 1, F.constant "insert" ), ( 1, F.constant "remove" ) ]


opsF =
    F.list (F.tuple3 ( opF, pidF, pidContentF )) |> F.map uniqueOps


applyOps =
    List.foldl applyOpTuple |> flip


uncurry3 f ( a, b, c ) =
    f a b c


applyOpTuple =
    uncurry3 applyOp


applyOp : String -> Pid -> PidContent -> Logoot -> Logoot
applyOp op =
    case op of
        "insert" ->
            insert

        "remove" ->
            remove

        _ ->
            \_ _ d -> d


all : Test
all =
    describe "elm-logoot test suite"
        [ describe "ops properties"
            [ fuzz opsF "idempotent" <|
                \ops ->
                    on Expect.equal Logoot.toDict (applyOps ops Logoot.empty) (applyOps ops Logoot.empty)
            , fuzz opsF "commutative" <|
                \ops ->
                    on Expect.equal
                        Logoot.toDict
                        (applyOps ops Logoot.empty)
                        (applyOps (List.reverse ops) Logoot.empty)
            , fuzz opsF "associative" <|
                \ops ->
                    let
                        d1 =
                            Logoot.empty |> applyOps ops |> applyOps (List.reverse ops)

                        d2 =
                            Logoot.empty |> applyOps (List.reverse ops) |> applyOps ops
                    in
                        on Expect.equal Logoot.toDict d1 d2
            ]
        ]
