import Dict as Dict exposing (..)
import Set as Set exposing (..)
import List as List exposing (..)

type alias Doc =
  { cemetery : Cemetery
  , content : Content
  }
type alias Cemetery = Dict Pid Int
type alias Content = Dict Pid PidContent
type alias Pid = (Positions, Clock)
type alias PidContent = String
type alias Positions = List Position
type alias Position = (Line, Site)
type alias Line = Int
type alias Site = Int
type alias Clock = Int

maxInt = 32000

initialPid : Pid
initialPid = ([(0,0)], 0)

lastPid : Pid
lastPid = ([(maxInt,0)], 0)

empty : Doc
empty =
  { cemetery = Dict.empty
  , content = Dict.fromList [ (initialPid, ""), (lastPid, "") ]
  }

insert : Pid -> PidContent -> Doc -> Doc
insert pid content doc =
  { doc | content = Dict.insert pid content doc.content}

remove : Pid -> Doc -> Doc
remove pid doc =
  { doc | content = Dict.remove pid doc.content}

comparePos : Positions -> Positions -> Order
comparePos posl posr =
  let
    compAll = (List.filter ((/=) EQ) >> take 1) (map2 compare posl posr)
  in
    case compAll of
      [] -> compare (length posl) (length posr)
      res -> Maybe.withDefault EQ (head res)

comparePid : Pid -> Pid -> Order
comparePid pidl pidr =
  case comparePos (fst pidl) (fst pidr) of
    EQ -> compare (snd pidl) (snd pidr)
    x -> x

zip : List a -> List b -> List (a,b)
zip = map2 (,)

generatePosition : Site -> Positions -> Positions -> Positions
generatePosition site posl posr =
  let
    comps =  zip posl posr
    loop = List.foldl (\(p1, p2) (folded, acc) ->
      if folded then (folded, acc)
      else case compare p1 p2 of
        EQ -> (False, acc ++ [p1])
        LT -> if fst p1 + 1 < snd p2 
          then (True, acc ++ [(fst p1 + 1, site)])
          else (True, acc ++ [p1, (0, site)])
        GT -> (True, acc ++ [p2, (0, site)])) (False, []) comps
    newPos = snd loop
  in
    if newPos == posl 
    then newPos ++ [(0, site)]
    else newPos
