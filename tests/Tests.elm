module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Dict
import Fuzz as F
import Logoot exposing (..)

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

pidBetweenTwo p1 p2 = pidBetween p1 p2 2 2

toNonEmptyPositions a = case a of
  [] -> [(0, 0)]
  _ -> a

natF = F.map abs F.int
lineF = natF
siteF = natF
clockF = natF
pidContentF = F.string
positionF = F.tuple (lineF, siteF)
positionsF = F.map toNonEmptyPositions (F.list positionF)
pidF = F.tuple (positionsF, clockF)

bigger p1 p2 =
  case comparePid p1 p2 of
    GT -> p1
    _ -> p2

smaller p1 p2 =
  case comparePid p1 p2 of
    LT -> p1
    _ -> p2

all : Test
all =
    describe "elm-logoot test suite"
      [ test "insert" <|
          \() ->
            let
              result = empty |> insert pids.small "hey!" |> toDict
              expected = Dict.fromList [ (initialPid, ""), (pids.small, "hey!"), (lastPid, "") ]
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
      , describe "pidBetween"
        [ test "simple position between" <|
            \() -> Expect.equal (pidBetweenTwo ([(1, 1)], 0) ([(5, 1)], 0)) ([(2, 2)], 2)
        , test "simple position nothing between" <|
            \() -> Expect.equal (pidBetweenTwo ([(1, 1)], 0) ([(2, 1)], 0)) ([(1, 1), (0, 2)], 2)
        , fuzz2 pidF pidF "generates pid bigger than the left pid" <|
           \p1 p2 ->
             let
               sm = smaller p1 p2
               bg = bigger p1 p2
               newPid = pidBetweenTwo sm bg
             in
               newPid
               |> comparePid sm
               |> Expect.equal LT
        ]
      ]
