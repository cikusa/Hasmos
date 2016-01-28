{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import System.Exit

import Reactive.Banana
import Reactive.Banana.Frameworks

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import qualified Graphics.Gloss.Interface.IO.Game as G

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Reader

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

type DtMoment r = (Member (Lift Moment) r, Member (Reader (Event Float)) r)

runLiftMoment :: MonadMoment m => Eff '[Lift Moment] w -> m w
runLiftMoment = liftMoment . runLift

timeAccumulator :: DtMoment r => Eff r (Behavior Float)
timeAccumulator = do
  dt <- ask
  accumB 0 $ (+) <$> dt

zoomingCircle :: DtMoment r => Float -> Color -> Eff r (Behavior Picture)
zoomingCircle r rawColor =
  fmap updateCircle <$> timeAccumulator
  where
    updateCircle t =
      let prog = abs (sin $ t * pi / 2)
          cirR = r * prog
          cirC = withAlpha prog rawColor in
      color cirC $ circleSolid cirR

mMain :: Event Float -> Event InputEvent -> MomentIO (Behavior Picture)
mMain dt input = do
  reactimate $ const exitSuccess <$> filterE isExitMsg input
  runLiftMoment $ runReader (zoomingCircle 200 $ makeColorI 191 202 230 255) dt
  where
    isExitMsg (EventKey (SpecialKey KeyEsc) Down _ _) = True
    isExitMsg _ = False

main :: IO ()
main = playBanana (InWindow "Hasmos" (1024, 768) (0, 0)) white 60 mMain
