-- |
-- Module      :  Data.StateMachines.Diagrams
-- License     :  BSD3
--
-- Maintainer  :  L0neGamer
-- Stability   :  experimental
--
-- For drawing diagrams of State machines.
module Data.StateMachines.Diagrams (drawStateMachineTo) where

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (AttributeEdge, AttributeNode)
import Data.GraphViz.Commands (GraphvizCommand (Dot))
import Data.List (intercalate)
import Data.Map as M (Map, assocs, fromList, fromListWith, lookup, (!))
import Data.Maybe (fromJust, fromMaybe)
import Data.Set as S (Set, fromList, insert, map, member, toList)
import Data.StateMachines.StateMachine
  ( State (Dead),
    StateID,
    StateLike (..),
    StateMachine (..),
    stateName',
  )
import Data.Tuple (swap)
import qualified Data.Vector as V
import Diagrams.Backend.SVG (renderPretty)
import Diagrams.Backend.SVG.CmdLine (B)
import Diagrams.Core (maxTraceP)
import Diagrams.Core.Names (IsName)
import Diagrams.Prelude
  ( Angle,
    ArrowOpts,
    Colour,
    Default (def),
    Diagram,
    Located (unLoc),
    P2,
    Path,
    QDiagram,
    V2,
    arrowBetween',
    arrowShaft,
    atop,
    bg,
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
    lightgrey,
    location,
    lw,
    mkWidth,
    named,
    none,
    pathTrails,
    place,
    rotate,
    small,
    text,
    traceP,
    turn,
    unitX,
    with,
    withName,
    yellow,
    (#),
    (&),
    (.+^),
    (.-.),
    (.~),
    (@@),
    (^/),
  )
import Diagrams.TwoD.GraphViz (getGraph, layoutGraph, mkGraph)
import Graphics.SVGFonts (fit_height, set_envelope, svgText)

-- | Draw a node based on the given state and colour.
node' :: State -> Colour Double -> Diagram B
node' internalName colour =
  text (stateName' internalName) # fontSizeL 17
    # fc black <> circle 19
    # named internalName
    # fc colour

-- | Get a colour for a node given the state, the start state, and the accept states.
chooseColour :: (Ord a, Floating a) => State -> State -> Set State -> Colour a
chooseColour Dead _ _ = grey
chooseColour state startState acceptStates
  | state `S.member` acceptStates && state == startState = cyan
  | state `S.member` acceptStates = yellow
  | state == startState = green
  | otherwise = blue

-- | Helper function that just wraps up other functions for easy node creation
nodeMatch :: State -> Set State -> State -> Diagram B
nodeMatch startState acceptStates s = node' s (chooseColour s startState acceptStates)

-- | Convert the usual @transitions@ type into a list of tuples denoting relations between
-- two states.
convertTransitions :: (StateLike s, Ord l, Ord e) => V.Vector (Map l (s StateID, e)) -> ([StateID] -> [State]) -> [(State, State, s (l, e))]
convertTransitions v mapNumbersToNames = temp
  where
    lst = zip (mapNumbersToNames [0 .. length v - 1]) (assocs <$> V.toList v)
    expandFromStart (ss, mps) = fmap (\(l, (sids, e)) -> (ss, sids, (l, e))) mps
    expandFromEnd (ss, sid, le) = fmap (\sid' -> ((ss, sid'), fromSingle le)) $ mapNumbersToNames $ S.toList $ toSet sid
    temp = fmap (\((s, s'), le) -> (s, s', le)) $ assocs $ M.fromListWith combineStateLike $ concatMap expandFromEnd $ concatMap expandFromStart lst

-- | Convert a given `Data.StateMachines.StateMachine.StateMachine` into a graph.
stateMachineToGraph :: (Ord l, Ord e, StateLike s, Monoid e) => ([StateID] -> [State]) -> StateMachine l s e -> Gr State (s (l, e))
stateMachineToGraph mapNumbersToNames StateMachine {..} =
  mkGraph
    (S.toList states)
    (convertTransitions transitions mapNumbersToNames)
  where
    states = S.insert Dead $ S.fromList $ mapNumbersToNames [0 .. length transitions -1]

-- | Convert a given `Data.StateMachines.StateMachine.StateMachine` into a `Diagram`.
-- This uses a lot of custom functions, so please investigate at your own risk.
stateMachineToDiagram :: (Ord l, Ord e, StateLike s, Show l, Show e, ShowTupleNoUnit e, Monoid e) => StateMachine l s e -> IO (Diagram B)
stateMachineToDiagram sm@StateMachine {..} =
  myDrawGraph
    (place . nodeMatch (numbersToNames M.! startStateID) acceptStates)
    (\nm1 point1 nm2 point2 e p d -> myArrowBetween (opts p) nm1 point1 nm2 point2 e d)
    <$> laidOutGraph
  where
    numbersToNames = M.fromList $ swap <$> M.assocs namesToNumbers
    mapNumbersToNames = ((numbersToNames M.!) <$>)
    graph = stateMachineToGraph mapNumbersToNames sm
    laidOutGraph = layoutGraph Dot graph
    acceptStates = S.map (numbersToNames M.!) acceptStateIDs
    opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p) & headLength .~ small

-- | A version of drawGraph' that has been modified to allow for specific customisations.
myDrawGraph ::
  (Ord v, Semigroup m) =>
  (v -> P2 Double -> QDiagram b V2 Double m) ->
  (v -> P2 Double -> v -> P2 Double -> e -> Path V2 Double -> QDiagram b V2 Double m -> QDiagram b V2 Double m) ->
  Gr (AttributeNode v) (AttributeEdge e) ->
  QDiagram b V2 Double m
myDrawGraph drawV drawE gr = compressedGraph
  where
    (vmap, edges) = getGraph gr
    drawE' (v1, v2, e, p) =
      drawE v1 (fromJust $ M.lookup v1 vmap) v2 (fromJust $ M.lookup v2 vmap) e p
    edgeFuncs = fmap drawE' edges
    nodeGraph = mconcat (fmap (uncurry drawV) (M.assocs vmap))
    compressedGraph = foldr (\ef d -> ef d) nodeGraph edgeFuncs

-- | Utility type class for showing tuples in a special way.
class ShowTupleNoUnit b where
  showTupleNoUnit :: (Show a, Show b) => (a, b) -> String

instance {-# OVERLAPPING #-} ShowTupleNoUnit () where
  showTupleNoUnit (x, ()) = show x

instance {-# OVERLAPPABLE #-} ShowTupleNoUnit b where
  showTupleNoUnit (x, y) = show x ++ ", " ++ show y

-- | Convert a StateLike object containing tuples of (l,e) into a diagram of the text
arrowText :: (StateLike s, Show l, Show e, ShowTupleNoUnit e) => s (l, e) -> Diagram B
arrowText les = annotateText str
  where
    les' = fmap showTupleNoUnit $ S.toList $ toSet les
    str
      | isSingle les = Prelude.head les'
      | otherwise = intercalate ", " $ fmap (\s -> '(' : s ++ ")") les'

-- | Create the text for a label for an arc, using special text drawing so it can have a
-- background.
annotateText :: String -> Diagram B
annotateText t = t # svgText def # fit_height 7 # set_envelope # lw none # bg lightgrey # fc black

-- | Draws an arrow between two named points. If their given coordinates are the same,
-- draw a self arc instead.
myArrowBetween :: (Ord v, IsName v, StateLike s, Show l, Show e, ShowTupleNoUnit e) => ArrowOpts Double -> v -> P2 Double -> v -> P2 Double -> s (l, e) -> Diagram B -> Diagram B
myArrowBetween opts nm1 point1 nm2 point2 e
  | point1 == point2 = myConnectPerim opts nm1 nm2 (1 / 12 @@ turn) (-1 / 12 @@ turn) annotateDiagram
  | otherwise = myConnectOutside opts nm1 nm2 annotateDiagram
  where
    annotateDiagram = arrowText e

-- | Taking heavily after `connectOutside'`. Used to draw between two locations and to
-- annotate the drawn arc.
myConnectOutside :: (IsName n1, IsName n2) => ArrowOpts Double -> n1 -> n2 -> Diagram B -> Diagram B -> Diagram B
myConnectOutside opts n1 n2 annotation =
  withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      let v = location b2 .-. location b1
          midpoint = location b1 .+^ (v ^/ 2)
          s' = fromMaybe (location b1) $ traceP midpoint (negate v) b1
          e' = fromMaybe (location b2) $ traceP midpoint v b2
       in atop (place annotation (s' .+^ (v ^/ 10))) . atop (arrowBetween' opts s' e')

-- | Taking heavily after `connectPerim'`. Used to draw a self arc and to annotate it.
myConnectPerim :: (IsName n1, IsName n2) => ArrowOpts Double -> n1 -> n2 -> Angle Double -> Angle Double -> Diagram B -> Diagram B -> Diagram B
myConnectPerim opts n1 n2 a1 a2 annotation =
  withName n1 $ \sub1 ->
    withName n2 $ \sub2 ->
      let [os, oe] = fmap location [sub1, sub2]
          s = fromMaybe os (maxTraceP os (unitX # rotate a1) sub1)
          e = fromMaybe oe (maxTraceP oe (unitX # rotate a2) sub2)
       in atop (place annotation ((s + e) / 2)) . atop (arrowBetween' opts s e)

-- | Draw a given state machine to the given path with the given width.
--
-- Good luck on trying to untangle the mess that went into this function.
drawStateMachineTo :: (StateLike s, Show l, Show e, Ord l, Ord e, Ord (s (l, e)), ShowTupleNoUnit e, Monoid e) => FilePath -> Double -> StateMachine l s e -> IO ()
drawStateMachineTo filePath width sm = do
  diag <- stateMachineToDiagram sm
  renderPretty filePath (mkWidth width) diag
