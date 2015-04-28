module Graphics.Gloss.Interface.FRP.Netwire (
  InputEvent,
  playWire,
  playWireIO,
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Control.Wire hiding ((.))
import Control.Wire.Unsafe.Event

-- | A synonym for Gloss' 'G.Event' to distinguish it from Netwire's
-- 'Event'.
type InputEvent = G.Event

-- | Gloss' 'play' with the event handling and updating provided by
-- the supplied 'Wire'.
playWire
  :: Display
  -> Color                      -- ^ Background color
  -> Int                        -- ^ Refresh rate
  -> (Wire (Timed Float ()) () Identity (Event InputEvent) Picture)
  -> IO ()
playWire disp bgColor hz wire0
  = play disp bgColor hz (Blank, wire0) fst handleEvents step
  where
    step dt (_, wire) = (pic, wire')
      where
        (ePic, wire') = runIdentity $ stepWire wire (Timed dt ()) (Right NoEvent)
        pic = case ePic of { Right x -> x ; Left _ -> Blank }
    handleEvents evt (pic0, wire) = (pic, wire')
      where
        (ePic, wire') = runIdentity $ stepWire wire (Timed 0 ()) (Right $ Event evt)
        pic = case ePic of { Right x -> x ; Left _ -> pic0 }

-- | Gloss' 'playIO' with the event handling and updating provided by
-- the supplied 'Wire'.
playWireIO
  :: Display
  -> Color                      -- ^ Background color
  -> Int                        -- ^ Refresh rate
  -> (Wire (Timed Float ()) () IO (Event InputEvent) Picture)
  -> IO ()
playWireIO disp bgColor hz wire0
  = playIO disp bgColor hz (Blank, wire0) (return . fst) handleEvents step
  where
    step dt (_, wire) = do
      (ePic, wire') <- stepWire wire (Timed dt ()) (Right NoEvent)
      let pic = case ePic of { Right x -> x ; Left _ -> Blank }
      return (pic, wire')
    handleEvents evt (pic0, wire) = do
      (ePic, wire') <- stepWire wire (Timed 0 ()) (Right $ Event evt)
      let pic = case ePic of { Right x -> x ; Left _ -> pic0 }
      return (pic, wire')
