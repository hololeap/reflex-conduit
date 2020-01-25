{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Reflex.Conduit (
      ResetConduitEventF (ResetConduitEventF)
    , ClearInputEventF (ClearInputEventF)
    , runConduitReflex)

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.NodeId (MonadNodeId)
import Data.Attoparsec.Text (Parser, string)
import Data.Char (toUpper)
import Data.Conduit.Attoparsec (ParseError, PositionRange, conduitParserEither)
import Data.Function ((&))
import Data.Functor (void)
import Reflex.Class ( Reflex, Behavior, Event, MonadHold, accumB, fmapMaybe
                    , leftmost, hold, never)
import Reflex.PerformEvent.Class (PerformEvent, Performable)
import Reflex.TriggerEvent.Class (TriggerEvent)
import Reflex.Vty.Host (VtyEvent)
import Reflex.Vty.Widget ( BoxStyle, VtyWidget, box, doubleBoxStyle, input
                         , keyCombo, mainWidget, splitV, text)
import Reflex.Vty.Widget.Layout(Orientation (Orientation_Column), fixed, runLayout)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Graphics.Vty as V

main :: IO ()
main = mainWidget $ do
    bs <- myBoxStyle
    charE   <- fmapMaybe vtyEventToChar <$> input
    clearE  <- void <$> keyCombo (V.KEsc, [])
    resultE <- parseEvent myParser clearE charE

    runLayout (pure Orientation_Column) 0 never $
        fixed (pure 5) $ box bs $ layout clearE charE resultE

    void <$> keyCombo (V.KChar 'c', [V.MCtrl])
  where
    myParser = string "abc"
    layout clearE charE resultE = 
        myVLayout
            (text "Matching string is \"abc\"; Press Esc to clear; Ctrl+C to quit")
            (inputWithClear clearE charE)
            (parseWidget clearE resultE)

-- | A line that shows the contents of `Event t a`.
-- | - Starts empty but updates whenever the second Event fires.
-- | - Clears when the first Event fires.
parseWidget :: (MonadHold t m, Reflex t, Show a)
    => Event t () -> Event t a -> VtyWidget t m ()
parseWidget clearE resultE = do
    let clearFE = "" <$ clearE
    let showFE  = showText <$> resultE
    pwE <- hold "" $ leftmost [clearFE, showFE]
    text pwE

-- | Creates an Event from `runConduitReflex` and `conduitParserEither`.
-- | If the first Event fires, it will reset the Conduit and clear the input buffer.
parseEvent :: (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), MonadIO m)
    => Parser a -> Event t () -> Event t Char -> m (Event t (Either ParseError (PositionRange, a)))
parseEvent p clearE charE =
    runConduitReflex 
        (ResetConduitEventF clearE) 
        (ClearInputEventF clearE)
        (T.singleton <$> charE) 
        (conduitParserEither p)
    
-- | A line that shows the keyboard input; clears when the first Event fires
inputWithClear :: (Reflex t, MonadHold t m, MonadFix m)
    => Event t () -> Event t Char -> VtyWidget t m ()
inputWithClear clearE charE = do
    let clearFE = const "" <$ clearE
    let snocFE  = flip T.snoc <$> charE
    inWCB <- accumB (&) "" $ leftmost [clearFE, snocFE]
    text inWCB

vtyEventToChar :: VtyEvent -> Maybe Char
vtyEventToChar = \case
    V.EvKey (V.KChar c) [V.MShift] -> Just $ toUpper c
    V.EvKey (V.KChar c) [        ] -> Just $         c
    _                              -> Nothing

myBoxStyle :: (Reflex t, MonadHold t m, MonadFix m) => m (Behavior t BoxStyle)
myBoxStyle = pure $ pure doubleBoxStyle

showText :: Show a => a -> Text
showText = T.pack . show

-- | Three lines supplied by VtyWidgets, with no focus
myVLayout :: (Reflex t, Monad m, MonadNodeId m)
    => VtyWidget t m a -> VtyWidget t m b -> VtyWidget t m c -> VtyWidget t m ()
myVLayout wa wb wc = void $ splitV constSize noFocus wa $ splitV constSize noFocus wb wc
  where
    constSize = pure (const 1)
    noFocus = pure (False, False)
