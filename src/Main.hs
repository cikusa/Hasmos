{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Main where

import System.Exit

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Gloss.Game hiding (Event)
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as G

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Reader

import Control.Arrow

import Data.Maybe

import Data.IORef
import Debug.Trace

type InputEvent = G.Event

playBanana
  :: Display
  -> Color
  -> Int
  -> (Event Float -> Event InputEvent -> MomentIO (Behavior Picture))
  -> IO ()
playBanana display color freq mPicture = do
  pictRef <- newIORef blank
  (tickHandler, tick)   <- newAddHandler
  (eventHandler, event) <- newAddHandler
  let networkDescription :: MomentIO ()
      networkDescription = do
        eTick    <- fromAddHandler tickHandler
        eEvent   <- fromAddHandler eventHandler
        bPicture <- mPicture eTick eEvent
        changes bPicture >>= reactimate' . fmap (fmap $ writeIORef pictRef)
  compile networkDescription >>= actuate
  playIO display color freq ()
    (const $ readIORef pictRef)
    (\ev _ -> () <$ event ev)
    (\dt _ -> () <$ tick  dt)

newtype Lift m a = Lift (m a)

lift :: Member (Lift m) r => m a -> Eff r a
lift = send . Lift

runLift :: Monad m => Eff '[Lift m] w -> m w
runLift (Val x) = return x
runLift (E u q) =
  case prj u of
    Just (Lift m) -> m >>= runLift . qApp q
    Nothing -> error "This should never happend."

instance (Member (Lift Moment) r) => MonadMoment (Eff r) where
  liftMoment = lift

runLiftMoment :: MonadMoment m => Eff '[Lift Moment] w -> m w
runLiftMoment = liftMoment . runLift

type GameMoment r =
  ( Member (Lift Moment) r
  , Member (Reader GameState) r )

data GameState = GameState
  { deltaTime  :: Event Float
  , windowSize :: Size }

getTick :: GameMoment r => Eff r (Event Float)
getTick = deltaTime <$> ask

createTimer :: GameMoment r => Eff r (Behavior Float)
createTimer = do
  eTick  <- getTick
  accumB 0 $ (+) <$> eTick

type Duration      = Float
type TimedObject a = (Duration, a)

timeline :: GameMoment r => Event [TimedObject a] -> (Float -> a -> Maybe a) -> Eff r (Behavior [TimedObject a])
timeline eobjs transform = do
  eTick <- getTick
  queue <-
    accumB ([], []) $ unionWith const
      (update  <$> eTick)
      (enqueue <$> eobjs)
  return $ fst <$> queue
  where
    update dt (acts, []) = (updateActs dt acts, [])
    update dt (acts, (d, v):xs)
      | d < 0     = (updateActs dt ((0, v):acts), xs)
      | otherwise = (updateActs dt acts, (d - dt, v):xs)
    updateActs dt = catMaybes . fmap updateAct
      where
        updateAct (ot, v) =
          let nt = ot + dt in (nt,) <$> transform nt v
    enqueue newobjs = second (++newobjs)

loadingScreen :: GameMoment r => Eff r (Behavior Picture)
loadingScreen = do
  eTick  <- getTick
  bTimer <- accumB 0 $ (+) <$> eTick
  return $ updateCircle <$> bTimer
  where
    updateCircle t =
      let prog = abs (sin $ t * pi / 2)
          cirR = fullRadius * prog
          cirC = withAlpha prog rawColor in
      color cirC $ circleSolid cirR
    rawColor = makeColorI 191 202 230 255
    fullRadius = 200

mMain :: Size -> Event Float -> Event InputEvent -> MomentIO (Behavior Picture)
mMain winSize dt input = do
  reactimate $ const exitSuccess <$> filterE isExitMsg input
  runLiftMoment $ runReader loadingScreen
    GameState
      { deltaTime  = dt
      , windowSize = winSize }
  where
    isExitMsg (EventKey (SpecialKey KeyEsc) Down _ _) = True
    isExitMsg _ = False

main :: IO ()
main = playBanana (InWindow "Hasmos" winSize (0, 0)) white 60 $ mMain winSizeF
  where
    winSize@(w, h) = (1024, 768)
    winSizeF = (fromIntegral w, fromIntegral h)
