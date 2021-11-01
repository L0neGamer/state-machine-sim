-- |
-- Module      :  Data.StateMachines.Diagrams
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- For drawing diagrams of State machines.
module Data.StateMachines.Diagrams where

import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Commands (GraphvizCommand (Dot))
import Data.Map as M (Map, assocs, fromList, fromListWith, (!))
import Data.Set as S (Set, fromList, insert, map, member, size, toList)
import Data.StateMachines.Examples
import Data.StateMachines.StateMachine (State (Dead, stateName), StateID, StateLike (combineStateLike, fromSingle, isSingle, toSet), StateMachine (..))
import Data.Tuple (swap)
import qualified Data.Vector as V
import Diagrams.Backend.SVG (renderPretty)
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Prelude
  ( Colour,
    Diagram,
    Located (unLoc),
    arrowBetween',
    arrowShaft,
    atPoints,
    black,
    blue,
    circle,
    cyan,
    fc,
    fontSizeL,
    gaps,
    green,
    grey,
    headLength,
    mkWidth,
    named,
    p2,
    pathTrails,
    place,
    regPoly,
    small,
    text,
    trailVertices,
    unp2,
    with,
    yellow,
    (#),
    (&),
    (.~),
  )
import Diagrams.TwoD.GraphViz (drawGraph, layoutGraph, mkGraph)
import System.Directory.Extra (getCurrentDirectory)

node' :: String -> String -> Colour Double -> Diagram B
node' txt internalName colour =
  text txt # fontSizeL 17
    # fc black <> circle 19
    # named internalName
    # fc colour

chooseColour :: (Ord a, Floating a) => State -> State -> Set State -> Colour a
chooseColour Dead _ _ = grey
chooseColour s s' ss
  | s `S.member` ss && s == s' = cyan
  | s `S.member` ss = yellow
  | s == s' = green
  | otherwise = blue

nodeMatch :: State -> Set State -> State -> Diagram B
nodeMatch startState acceptStates s@Dead = node' (show s) (show s) (chooseColour s startState acceptStates)
nodeMatch startState acceptStates s = node' (stateName s) (show s) (chooseColour s startState acceptStates)

createNodes :: Set State -> State -> Set State -> Diagram B
createNodes states startState acceptStates = atPoints (trailVertices $ regPoly (S.size states) 1) (nodeMatch startState acceptStates <$> S.toList states)

-- createGraph :: (StateLike s, Show l, Show e) => Set State -> Set State -> Set (State, State, s (l, e)) -> Diagram B
-- createGraph states acceptStates transitions' = baseGraph # arrows
--   where
--     baseGraph = createNodes states acceptStates
--     toArrow :: (Show l, Show e, StateLike s) => (State, State, s (l, e)) -> Diagram B -> Diagram B
--     toArrow (s, s', les) = \b -> connectOutside (show s) (show s') b <> text (arrowText les) # fontSizeL 0.05
--     arrows = applyAll $ fmap toArrow (S.toList transitions')

arrowText :: (StateLike s, Show l, Show e) => s (l, e) -> (String, Diagram B)
arrowText les = (str, fontSizeL 5 $ text str)
  where
    les' = S.toList $ toSet les
    str
      | isSingle les = show $ Prelude.head les'
      | otherwise = show les'

convertTransitions :: (StateLike s, Ord l, Ord e, Ord (s (l, e))) => V.Vector (Map l (s StateID, e)) -> ([StateID] -> [State]) -> Set (State, State, s (l, e))
convertTransitions v mapNumbersToNames = S.fromList temp
  where
    lst = zip (mapNumbersToNames [0 .. length v - 1]) (assocs <$> V.toList v)
    expandFromStart (ss, mps) = fmap (\(l, (sids, e)) -> (ss, sids, (l, e))) mps
    expandFromEnd (ss, sid, le) = fmap (\sid' -> ((ss, sid'), fromSingle le)) $ mapNumbersToNames $ S.toList $ toSet sid
    temp = fmap (\((s, s'), le) -> (s, s', le)) $ assocs $ M.fromListWith combineStateLike $ concatMap expandFromEnd $ concatMap expandFromStart lst

stateMachineToGraph :: (Ord l, Ord e, Ord (s (l, e)), StateLike s) => ([StateID] -> [State]) -> StateMachine l s e -> Gr State (s (l, e))
stateMachineToGraph mapNumbersToNames StateMachine {..} = mkGraph (S.toList states) (S.toList $ convertTransitions transitions mapNumbersToNames)
  where
    states = S.insert Dead $ S.fromList $mapNumbersToNames [0 .. length transitions -1]

stateMachineToDiagram :: (Ord l, Ord e, Ord (s (l, e)), StateLike s, Show l, Show e) => StateMachine l s e -> IO (Diagram B)
stateMachineToDiagram sm@StateMachine {..} =
  drawGraph (place . nodeMatch (numbersToNames M.! startStateID) acceptStates) (\_ point1 _ point2 e p -> arrowAnnotate point1 point2 e <> arrowBetween' (opts p) point1 point2) <$> layoutGraph Dot graph
  where
    numbersToNames = M.fromList $ swap <$> M.assocs namesToNumbers
    mapNumbersToNames = ((numbersToNames M.!) <$>)
    graph = stateMachineToGraph mapNumbersToNames sm
    acceptStates = S.map (numbersToNames M.!) acceptStateIDs
    opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p) & headLength .~ small
    arrowAnnotate point1 point2 e = place dia (p2 (signum (point2y - point1y) * (5 + fromIntegral (length str)), 0) + (point1 * 2 + point2) / p2 (3, 3))
      where
        (_, point1y) = unp2 point1
        (_, point2y) = unp2 point2
        (str, dia) = arrowText e

-- stateMachineToDiagram :: (StateLike s, Show l, Show e, Ord l, Ord e, Ord (s (l, e))) => StateMachine l s e -> Diagram B
-- stateMachineToDiagram StateMachine {..} = createGraph states acceptStates (convertTransitions transitions mapNumbersToNames)
--   where
--     numbersToNames = M.fromList $ swap <$> M.assocs namesToNumbers
--     mapNumbersToNames = ((numbersToNames M.!) <$>)
--     states = S.insert Dead $ S.fromList $mapNumbersToNames [0 .. length transitions -1]
--     acceptStates = S.map (numbersToNames M.!) acceptStateIDs

test :: (StateLike s, Show l, Show e, Ord l, Ord e, Ord (s (l, e))) => StateMachine l s e -> IO ()
test sm = do
  curDir <- getCurrentDirectory
  -- print curDir
  diag <- stateMachineToDiagram sm
  renderPretty (curDir ++ "/out.svg") (mkWidth 500) diag

-- text' :: Double -> String -> Diagram B
-- text' d s = text s # fontSizeL d # lw none  # fc black

-- stateLabel :: String -> Diagram B
-- stateLabel = text' 6
-- arrowLabel :: String -> Diagram B
-- arrowLabel txt size = text' size txt