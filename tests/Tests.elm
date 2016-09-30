module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Dict
import Fuzz as F
import Logoot exposing (..)

maxInt : Int
maxInt = 32000

firstPid : Pid
firstPid = ([(0,0)],0)

lastPid : Pid
lastPid = ([(maxInt,0)],0)

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g a b = f (g a) (g b)

pids =
  { small         = ([(1, 1)], 0)
  , smallTwo      = ([(1, 1), (2, 2)], 0)
  , medium        = ([(2, 2)], 0)
  , mediumTwo     = ([(2, 2), (3, 4)], 0)
  , mediumMany    = ([(2, 2), (3, 4), (5, 6), (7, 8)], 0)
  , big           = ([(30, 5)], 0)
  , bigTwo        = ([(30, 5), (40, 7)], 0)
  , bigTwoSmaller = ([(30, 5), (35, 6)], 0)
  }

posBetweenTwo = posBetween 2

toNonEmptyPositions a = case a of
  [] -> [(1, 0)]
  _ -> a

uniqueOps = Dict.values << List.foldl (\(o,p,c) -> Dict.insert p (o,p,c)) Dict.empty

natF = F.intRange 1 100
lineF = natF
siteF = natF
clockF = natF
pidContentF = F.string
positionF = F.tuple (lineF, siteF)
positionsF = F.list positionF |> F.map toNonEmptyPositions 
pidF = F.tuple (positionsF, clockF)

opF = F.frequencyOrCrash [ (1, F.constant "insert"), (1, F.constant "remove") ]
opsF = F.list (F.tuple3 (opF, pidF, pidContentF)) |> F.map uniqueOps

applyOps = List.foldl applyOpTuple |> flip

uncurry3 f (a, b, c) = f a b c
applyOpTuple = uncurry3 applyOp

applyOp : String -> Pid -> PidContent -> Logoot -> Logoot
applyOp op =
  case op of
    "insert" -> insert
    "remove" -> remove
    _ -> \_ _ d -> d

bigger p1 p2 =
  case comparePos p1 p2 of
    GT -> p1
    _ -> p2

smaller p1 p2 =
  case comparePos p1 p2 of
    LT -> p1
    _ -> p2

all : Test
all =
    describe "elm-logoot test suite"
      [ test "insert" <|
          \() ->
            let
              result = empty |> insert pids.small "hey!" |> toDict
              expected = Dict.fromList [ (firstPid, ""), (pids.small, "hey!"), (lastPid, "") ]
            in
              Expect.equal result expected
      , describe "comparePid"
        [ describe "equal"
          [ test "" <| \() -> Expect.equal (comparePid pids.small pids.small) EQ
          , test "" <| \() -> Expect.equal (comparePid pids.smallTwo pids.smallTwo) EQ
          , test "" <| \() -> Expect.notEqual (comparePid pids.small pids.smallTwo) EQ
          , test "" <| \() -> Expect.notEqual (comparePid pids.small pids.medium) EQ
          , test "" <| \() -> Expect.notEqual (comparePid pids.bigTwo pids.smallTwo) EQ
          ]

        , describe "bigger"
          [ test "" <| \() -> Expect.equal (comparePid pids.smallTwo pids.small) GT
          , test "" <| \() -> Expect.equal (comparePid pids.medium pids.small) GT
          , test "" <| \() -> Expect.equal (comparePid pids.big pids.medium) GT
          , test "" <| \() -> Expect.equal (comparePid pids.bigTwo pids.bigTwoSmaller) GT
          , test "" <| \() -> Expect.equal (comparePid pids.big pids.smallTwo) GT
          ]

        , describe "smaller"
          [ test "" <| \() -> Expect.equal (comparePid pids.small pids.big) LT
          , test "" <| \() -> Expect.equal (comparePid pids.small pids.smallTwo) LT
          , test "" <| \() -> Expect.equal (comparePid pids.medium pids.mediumTwo) LT
          , test "" <| \() -> Expect.equal (comparePid pids.medium pids.big) LT
          , test "" <| \() -> Expect.equal (comparePid pids.medium pids.bigTwoSmaller) LT
          , test "" <| \() -> Expect.equal (comparePid pids.big pids.bigTwoSmaller) LT
          , test "" <| \() -> Expect.equal (comparePid pids.bigTwoSmaller pids.bigTwo) LT
          ]
        ]
      , describe "posBetween"
        [ test "simple position between" <|
            \() -> Expect.equal (posBetween 2 [(1, 1)] [(5, 1)]) [(2, 2)]
        , test "simple position nothing between" <|
            \() -> Expect.equal (posBetween 2 [(1, 1)] [(2, 1)]) [(1, 1), (0, 2)]
        , fuzz2 positionsF positionsF "generates pos bigger than the left pos" <|
           \p1 p2 ->
             let
               sm = smaller p1 p2
               bg = bigger p1 p2
               newPid = posBetween 2 sm bg
             in
               if p1 == p2 then Expect.pass
               else newPid
               |> comparePos sm
               |> Expect.equal LT
        , fuzz2 positionsF positionsF "generates pos smaller than the right pos" <|
           \p1 p2 ->
             let
               sm = smaller p1 p2
               bg = bigger p1 p2
               newPid = posBetween 2 sm bg
             in
               if p1 == p2 then Expect.pass
               else newPid
               |> comparePos bg
               |> Expect.equal GT
        ]
      , describe "ops properties"
        [ fuzz opsF "idempotent" <|
          \ops ->
            on Expect.equal Logoot.toDict (applyOps ops Logoot.empty) (applyOps ops Logoot.empty)
        , fuzz opsF "commutative" <|
          \ops ->
            on Expect.equal Logoot.toDict
              (applyOps ops Logoot.empty)
              (applyOps (List.reverse ops) Logoot.empty)
        , fuzz opsF "associative" <|
          \ops ->
            let
              d1 = Logoot.empty |> applyOps ops |> applyOps (List.reverse ops)
              d2 = Logoot.empty |> applyOps (List.reverse ops) |> applyOps ops
            in
              on Expect.equal Logoot.toDict d1 d2
        ]
      ]
