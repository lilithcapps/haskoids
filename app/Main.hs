{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Boid
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception        (bracket_)
import           Control.Monad            (forM, forM_)
import           Data.Maybe               (fromMaybe)
import           ReadArgs                 (readArgs)
import qualified System.Console.ANSI      as C
import           System.IO                (hFlush, stdout)
import qualified System.Random            as R

type ColoredFrame = [(IO (), Boid)]
type Frame = [Boid]

main :: IO ()
main = do
  (count :: Maybe Int, maybeFps :: Maybe Int) <- readArgs
  maybeSize <- C.getTerminalSize
  let (yMax, xMax) = fromMaybe (100, 100) maybeSize
  let boidCount = fromMaybe 100 count
  let fps = fromIntegral $ fromMaybe 60 maybeFps :: Double
  let colors = [C.Black, C.Blue, C.Cyan, C.Green, C.Magenta, C.Red, C.White, C.Yellow]
  let intensities = [C.Vivid, C.Dull]
  gen <- R.getStdGen
  let xPos =  R.randomRs (0, fromIntegral xMax) gen :: [Float]
  let yPos =  R.randomRs (0, fromIntegral yMax) gen :: [Float]
  let xVel =  R.randomRs (-1.0, 1.0) gen :: [Float]
  let yVel =  R.randomRs (-1.0, 1.0) gen :: [Float]
  let color = R.randomRs (0, length colors - 1) gen :: [Int]
  let intense = R.randomRs (0, length intensities - 1) gen :: [Int]

  let boids = [
        Boid (xp, yp) (xv, yv)
        | xp <- xPos
        | yp <- yPos
        | xv <- xVel
        | yv <- yVel
        ]

  let setColorFuncs = [
        C.setSGR [C.SetColor C.Foreground (intensities !! i) (colors !! c)]
        | c <- color
        | i <- intense
        ]

  let generator :: (a -> a) -> a -> [a]
      generator f s = s : generator f (f s)

  let coloredBoids = zip setColorFuncs (take boidCount boids)
  let frames = generator (nextFrame (xMax, yMax)) coloredBoids

  C.hideCursor
  C.enableLineWrap
  C.setTitle "Haskoids"
  bracket_ C.useAlternateScreenBuffer (C.useNormalScreenBuffer >> C.showCursor)
    $ forM_ frames (\bs -> do
      -- C.clearScreen
      let outs = C.clearScreen >> forM bs (\(setColor, b)
            ->  let (col, row) = position $! b
                        in C.setCursorPosition (round row) (round col)
                        >> setColor
                        >> putChar '*') >> hFlush stdout
      let pause = threadDelay $ round $ 1e6 / fps -- N frames per second (1e6 micros) ==>
                                                 -- 1e6/N seconds / frame for
                                                 -- time to show each frame

      concurrently pause outs
      )
  where
    xMin = 0
    yMin = 0

    maxSpeed = 4.0
    minSpeed = 0.1

    visionDist = 10.0

    cohesionStr = 0.005

    separationDist = 2.0
    separationStr = 0.05

    alignmentStr = 0.05

    nextFrame :: (Int, Int) -> ColoredFrame -> ColoredFrame
    nextFrame s f =
      let frame = map snd f in
      fmap (newPosition s frame) <$> f

    newPosition ::(Int, Int) -> Frame -> Boid -> Boid
    newPosition (xMax, yMax) frame b =
      let f = filter notSelf frame in
      let vis = filter visible f in
      applyVelocity .
      applyBounds $
        doSeparation f .
        doCohesion vis .
        doAlignment vis
        $ b
      where
        applyVelocity :: Boid -> Boid
        applyVelocity b' =
          b' {position = (clamp . velocity) b' + position b'}
              where
                clamp :: MovementVector -> MovementVector
                clamp v
                  | magnitude v > maxSpeed = mult maxSpeed . unit $ v
                  | magnitude v < minSpeed = mult minSpeed . unit $ v
                  | otherwise = v

        applyBounds :: Boid -> Boid
        applyBounds b' =
          let pos = position b' in
            b' { velocity = keepBounds pos . velocity $ b'}
            where
              keepBounds :: PositionVector -> MovementVector -> MovementVector
              keepBounds p = keepYBounds p . keepXBounds p
              keepXBounds p v
                | x p < xMin = (0.1, y v)
                | x p > fromIntegral xMax = (- 0.1, y v)
                | otherwise = v
              keepYBounds p v
                | y p < yMin = (x v, 0.1)
                | y p > fromIntegral yMax = (x v, - 0.1)
                | otherwise = v

        -- move away from other boids..
        doSeparation :: Frame -> (Boid -> Boid)
        doSeparation f = \a -> a { velocity = separationStr `mult` vel + velocity a}
          where
            vel :: MovementVector
            vel = sum $ map ((-) (position b) . position) filtered
            filtered :: [Boid]
            filtered = filter ((>) (separationDist ** 2) . dist b) f

        -- move towards center of other boids..
        doCohesion :: Frame -> (Boid -> Boid)
        doCohesion f = \a -> a { velocity = cohesionStr `mult` vel + velocity a}
          where
            vel :: MovementVector
            vel = avg (map position f) - position b

        -- move in the same direction as other boids..
        doAlignment :: Frame -> (Boid -> Boid)
        doAlignment f = \a -> a { velocity = alignmentStr `mult` vel + velocity a}
          where
            vel :: MovementVector
            vel = avg (map velocity f) - velocity b

        visible :: Boid -> Bool
        visible b1 = dist b b1 < (visionDist ** 2)

        notSelf :: Boid -> Bool
        notSelf b1 = dist b b1 /= 0

        avg :: [Vector] -> Vector
        avg l = (1.0 / fromIntegral (length l)) `mult` sum l
