{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
-- # LANGUAGE DeriveAnyClass #
module Bhut (Body(..), mass, position, velocity, doUpdate, Quadtree(..)) where
import Data.Maybe
import Data.List
import SDL.Vect(V2(..))
import Control.Lens hiding (element, without)
import Control.Lens.TH
import Control.Exception
import Debug.Trace

type Vector = V2 Rational
data Body = Body
            { _mass :: Rational
            , _position :: Vector
            , _velocity :: Vector } deriving (Show, Eq)
$(makeLenses ''Body)

computeCenter :: [Body] -> Vector
computeCenter bodies =
  let totalMass = foldl1' (+) $ map (\body -> body ^. mass) bodies
      weightedsum = foldl1' (+) $ map (\body -> (body ^. position) * fromRational (body ^. mass)) bodies
  in weightedsum / fromRational totalMass

type Extent = Maybe (Rational, Rational, Rational, Rational)

inExtentDec :: Body -> Extent -> Bool
inExtentDec _ Nothing = False
inExtentDec Body{_position = V2 x y}
            (Just(xmin, xmax, ymin, ymax)) =
  xmin <= x && x <= xmax && ymin <= y && y <= ymax

computeExtent :: [Body] -> Extent
computeExtent [] = Nothing
computeExtent (body:bodies) =
  let V2 x y = body ^. position in
  case computeExtent bodies of
    Just (xmin,xmax,ymin,ymax) -> Just (min xmin x, max xmax x, min ymin y, max ymax y)
    Nothing -> Just (x, x, y, y)

data Quadtree = Quadtree
                {
                  _body :: Maybe Body -- body stored here in the case of a leaf node
                , _extent :: Extent
                , _treemass :: Rational -- total mass of the tree in kilograms
                , _treecenter :: Maybe Vector
                , _q1 :: Maybe Quadtree
                , _q2 :: Maybe Quadtree
                , _q3 :: Maybe Quadtree
                , _q4 :: Maybe Quadtree
                } deriving (Show)
$(makeLenses ''Quadtree)

emptyQuadtree :: Extent -> Quadtree
emptyQuadtree e = Quadtree { _body = Nothing,
                             _extent = e,
                             _treemass = 0,
                             _treecenter = Nothing,
                             _q1 = Nothing,
                             _q2 = Nothing,
                             _q3 = Nothing,
                             _q4 = Nothing }

allBodies :: Quadtree -> [Body]
allBodies tree =
  let rest = concat $ map allBodies $ catMaybes [tree ^. q1, tree ^. q2, tree ^. q3, tree ^. q4] in
    case tree ^. body of Nothing -> []
                         Just b -> [b] ++ rest

insertInFirstFit :: Body -> [Quadtree] -> Maybe [Quadtree]
insertInFirstFit b [] =
  Nothing
insertInFirstFit b (t:ts) =
 if inExtentDec b (t ^. extent) then
   do
     inserted <- (insertBody b t)
     pure $ inserted:ts
 else do
   inserted <- insertInFirstFit b ts
   pure $ t:inserted

insertBody :: Body -> Quadtree -> Maybe Quadtree
insertBody b tree =
  if inExtentDec b (tree ^. extent) then
    case (tree ^. body) of
      Nothing ->
        -- ez mode
        Just $ tree & body .~ (Just b)
      Just b' ->
        let subtrees = quadrantTrees tree in
        -- HACK: this is kind of awkward
        do
          subtrees <- insertInFirstFit b subtrees
          subtrees <- insertInFirstFit b' subtrees
          let (q1':q2':q3':q4':[]) = subtrees
              newTree = (tree &~ do
                                q1 .= Just q1'
                                q2 .= Just q2'
                                q3 .= Just q3'
                                q4 .= Just q4')
          Just $ newTree &~
            let bodies = allBodies newTree in
            do treecenter .= (Just $ computeCenter bodies)
               treemass .= foldl1' (+) (bodies ^.. (traverse . mass))
               extent .= computeExtent bodies
  else
    trace ((show b) ++ " not in extent " ++ (show (tree ^. extent))) Nothing
    -- Nothing



quadrantTrees :: Quadtree -> [Quadtree]
quadrantTrees Quadtree { _extent = Nothing } = []
quadrantTrees Quadtree { _extent = Just (xmin, xmax, ymin, ymax) } =
  let xavg = (xmin + xmax) / 2
      yavg = (ymin + ymax) / 2

      q1Extent = Just (xavg, xmax, yavg, ymax)
      q2Extent = Just (xmin, xavg, yavg, ymax)
      q3Extent = Just (xmin, xavg, ymin, yavg)
      q4Extent = Just (xavg, xmax, ymin, yavg)

  in [ emptyQuadtree e
     | e <- [q1Extent, q2Extent, q3Extent, q4Extent]]


-- TODO: this implementation is totally wrong and has awful runtime
--       characteristics; use a more conventional strategy
buildQuadtree :: [Body] -> Maybe Quadtree
buildQuadtree [] = Just $ emptyQuadtree Nothing
-- buildQuadtree [body] = Just $ singletonQuadtree body
buildQuadtree (body:bodies) =
  let initialQuadtree = Quadtree { _body = Nothing
                                 , _extent = computeExtent (body:bodies)
                                 , _treemass = foldl1' (+) $ map (^. mass) bodies
                                 , _treecenter = Just $ computeCenter bodies
                                 , _q1 = Nothing
                                 , _q2 = Nothing
                                 , _q3 = Nothing
                                 , _q4 = Nothing }
  in foldl' (\qtree body -> do qtree <- qtree
                               let Just inserted = insertBody body qtree
                               return inserted)
            (Just initialQuadtree)
            bodies


withoutBody :: Quadtree -> Body -> Maybe Quadtree
withoutBody tree b =
  case (tree ^. body) of
    Nothing ->
      do q1' <- tree ^. q1
         q2' <- tree ^. q2
         q3' <- tree ^. q3
         q4' <- tree ^. q4
         q1' <- withoutBody q1' b
         q2' <- withoutBody q2' b
         q3' <- withoutBody q3' b
         q4' <- withoutBody q4' b
         return $ (tree &~ do
                     q1 .= Just q1'
                     q2 .= Just q2'
                     q3 .= Just q3'
                     q4 .= Just q4')
    _ ->
      Just $ tree & body %~ excludeb
        where excludeb b' =
                do b' <- b'
                   if b' == b then Nothing else (Just b)

-- fixed accuracy parameter, roughly 1
theta :: Rational
theta = 1

euclideanDist :: Vector -> Vector -> Rational
euclideanDist (V2 x1 y1) (V2 x2 y2) =
  toRational $ sqrt $ fromRational $ (x1 - x2)^2 + (y1 - y2)^2

magnitude :: Vector -> Rational
magnitude = euclideanDist $ V2 0 0

-- computes force of gravity between two bodies as a vector indicating
-- the force applied on object 1 by object 2
newtonianForce :: Body -> Body -> Vector
newtonianForce Body{_mass=m1, _position=pos1} Body{_mass=m2, _position=pos2, _velocity = 0} =
  unit * fromRational ((gConst * m1*m2) / r^2) -- (N m^2 / kg^2) * kg^2 / m^2 = N
  where gConst = 6.674 * (toRational $ 10**(-11)) -- (N m^2) / kg^2
        r = euclideanDist pos1 pos2 -- m
        unit = (pos2 - pos1) * fromRational (magnitude $ pos2 - pos1)

computeForce :: Body -> Maybe Quadtree -> Vector
computeForce b tree =
  case tree of
    Nothing -> V2 0 0
    Just tree ->
      case tree ^. extent of
        Nothing -> V2 0 0
        Just (xmin, xmax, ymin, ymax) -> 
          let Just center = tree ^. treecenter
              diameter = euclideanDist center (b ^. position)
              cellLength = max (abs $ xmin - xmax) (abs $ ymin - ymax)
          in
            if cellLength / diameter < theta then
              newtonianForce b Body{_mass = tree ^. treemass , _position = center, _velocity = 0}
            else
              foldl1' (+) $ [ computeForce b subtree
                            | subtree <- [tree ^. q1 , tree ^. q2 , tree ^. q3 , tree ^. q4]]

nextVelocity :: Body -> Vector -> Rational -> Vector
nextVelocity b force time =
  let v0 = b ^. velocity
      accel = force / fromRational (b ^. mass)
  in
    v0 + accel * fromRational time

rat :: Rational -> V2 Rational
rat = fromRational

nextPosition :: Body -> Vector -> Rational -> Vector
nextPosition b force time =
  let pos0 = b ^. position
      accel = force / rat (b ^. mass)
      v0 = b ^. velocity
  in
    pos0 + v0* rat time + accel * rat (1/2 * time^2)

doUpdate :: Rational -> [Body] -> Maybe [Body]
doUpdate time (b:bs) =
  do
    tree <- buildQuadtree (b:bs)
    rest <- doUpdate time bs
    let
      force = computeForce b (withoutBody tree b)
      nextVel = nextVelocity b force time
      nextPos = nextPosition b force time
      nextBody = b &~ do
                  position .= nextPos
                  velocity .= nextVel
     in Just $ nextBody:rest

doUpdate _ [] = Just []
