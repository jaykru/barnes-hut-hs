{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where
-- import Criterion.Main
import Bhut (doUpdate, mass, Body(..), position, velocity, Quadtree(..))
import Control.Concurrent
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.IORef
import Data.Maybe
import Debug.Trace
import SDL hiding (trace)
import SDL.Vect(V2(..), V4(..))
import SDL.Primitive(fillCircle)
import Foreign.C.Types

import qualified Data.Text as T
import Control.Monad (unless, forever, void)

test_bodies = [ Body { _mass = 10,
                       _position = V2 100 100,
                       _velocity = V2 0 0 }
              , Body { _mass = 15,
                       _position = V2 100 10,
                       _velocity = V2 0 0 }
              , Body { _mass = 10,
                       _position = V2 500 500,
                       _velocity = V2 0 0 }
              , Body { _mass = 10,
                       _position = V2 700 700,
                       _velocity = V2 0 0 } ]

toCInt :: Rational -> CInt
toCInt r = (round . fromRational) r

adjustToOrigin :: Renderer -> V2 Rational -> IO (V2 Rational)
adjustToOrigin renderer (V2 x y) = do
  viewport <- get $ rendererLogicalSize renderer
  case viewport of
    Nothing -> pure $ (V2 x y)
    Just (V2 w h) ->
      do
        let !blah = trace "viewport" (V2 w h)
        return $ V2 (x + (toRational w)) (y + (toRational h))
  
renderBodies :: Renderer -> [Body] -> IO ()
renderBodies renderer bodies = do
  rendererDrawColor renderer $= V4 0 0 0 255 -- black
  clear renderer

  rendererDrawColor renderer $= V4 0 255 0 255 -- green
  let points = map (P . fmap toCInt . _position) bodies
  putStrLn $ "points: " ++ show points  
  adjusted <-  pure -- $ mapM id
  -- (adjustToOrigin renderer)
                    $ map _position bodies
  putStrLn $ "adjusted: " ++ (show (map (P . fmap toCInt) adjusted))
  mapM_ (\pos -> fillCircle renderer pos 4 (V4 0 255 0 255)) $ map (fmap toCInt) adjusted
  present renderer

renderQuadtree :: Renderer -> Quadtree -> IO ()
renderQuadtree _ Quadtree{_extent = Nothing} = return ()
renderQuadtree renderer Quadtree{_extent = Just(xmin, xmax, ymin, ymax)} =
  do
    return ()
    -- rendererDrawColor renderer $= V4 0 0 0 255 -- black
    -- clear renderer

    -- rendererDrawColor renderer $= V4 0 255 0 255 -- green
    -- let points = map (P . fmap toCInt . position) bodies
    -- putStrLn $ "points: " ++ show points
    -- adjusted <-  pure -- $ mapM id
    -- -- (adjustToOrigin renderer)
    --                 $ map position bodies
    -- putStrLn $ "adjusted: " ++ (show (map (P . fmap toCInt) adjusted))
    -- mapM_ (\pos -> fillCircle renderer pos 4 (V4 0 255 0 255)) $ map (fmap toCInt) adjusted
    -- present renderer

mainLoop :: _ -> IORef [Body] -> IO ()
mainLoop renderer bodies = do
    _ <- pollEvents
    bs <- readIORef bodies
    putStrLn "next iter"
  -- putStrLn $ show bs
    renderBodies renderer bs
    let Just newBodies = doUpdate 50000 bs
    writeIORef bodies newBodies

main :: IO ()
main = do
  initializeAll
  window <- createWindow (T.pack "physics go brr") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  bodies <- newIORef test_bodies

  forever $ mainLoop renderer bodies
  -- return ()

-- main =
--   defaultMain [
--         bgroup "mainLoop" [ bench "mainLoop" $ whnfAppIO mainLoop test_bodies ]
--   ]
