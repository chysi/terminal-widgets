{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Widgets.Common where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Coerce
import Data.Monoid hiding (Alt)
import Data.Text qualified as Text
import Prettyprinter
import System.Terminal.Render qualified as Render
import Prelude

data Token m
    = TText Int Text
    | TLine
    | TAnnPush (Attribute m)
    | TAnnPop
    deriving stock (Generic)

tokenise :: forall w m. (Widget w, MonadTerminal m) => w -> [Token m]
tokenise = go . layoutPretty defaultLayoutOptions . toDoc
  where
    go :: SimpleDocStream (Attribute m) -> [Token m]
    go SFail = []
    go SEmpty = []
    go (SChar c rest) = TText 1 (Text.singleton c) : go rest
    go (SText len text rest) = TText len text : go rest
    go (SLine indentation rest) = TLine : TText indentation (Text.replicate indentation " ") : go rest
    go (SAnnPush ann rest) = TAnnPush ann : go rest
    go (SAnnPop rest) = TAnnPop : go rest

(#.) :: (Coercible c b) => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce (\x -> x :: b) :: forall a b. (Coercible b a) => a -> b

type Getting r s a = (a -> Const r a) -> s -> Const r s

(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)

is :: a -> Getting (First c) a c -> Bool
a `is` c = isJust $ a ^? c

class Widget w where
    cursor :: Lens' w Position
    handleEvent :: Event -> w -> w
    {-# MINIMAL handleEvent, cursor #-}
    submitEvent :: w -> Maybe Event
    submitEvent w
        | valid w = Just $ KeyEvent EnterKey []
        | otherwise = Nothing
    valid :: w -> Bool
    valid = const True
    toDoc :: (MonadTerminal m) => w -> Doc (Attribute m)
    default toDoc :: (Show w) => w -> Doc (Attribute m)
    toDoc = ishow
    lineCount :: w -> Int
    lineCount =
        (1 +)
            . length
            . filter (`is` #_TLine)
            . tokenise @w @(TerminalT LocalTerminal IO)
    render :: forall m. (MonadTerminal m) => (Maybe w, w) -> m ()
    render = defaultRender

defaultRender :: forall w m. (Widget w, MonadTerminal m) => (Maybe w, w) -> m ()
defaultRender (maybeOld, new) = Render.render (r <$> maybeOld) (r new)
  where
    r :: w -> (Position, Doc (Attribute m))
    r w = (w ^. cursor, toDoc w)

data Modifier = Shift | Ctrl | Alt | Meta
    deriving stock (Bounded, Enum)

toModifiers :: Modifier -> Modifiers
toModifiers Shift = shiftKey
toModifiers Ctrl = ctrlKey
toModifiers Alt = altKey
toModifiers Meta = metaKey

instance IsList Modifiers where
    type Item Modifiers = Modifier
    fromList = mconcat . fmap toModifiers
    toList mods = filter hasMod [minBound .. maxBound]
      where
        hasMod c = mods .&. toModifiers c /= mempty

runWidget'
    :: forall m w
     . (MonadTerminal m, Widget w)
    => ((Maybe w, w) -> m ())
    -> ((Maybe w, w) -> m ())
    -> w
    -> m w
runWidget' preRender postRender = go Nothing
  where
    cleanup :: w -> m ()
    cleanup w = do
        let dy = lineCount w - (w ^. cursor . #row) - 1
        when (dy > 0) $ moveCursorDown dy
        putLn
        resetAttributes
        showCursor
    go :: Maybe w -> w -> m w
    go maybeOld current = do
        preRender (maybeOld, current)
        render (maybeOld, current)
        postRender (maybeOld, current)
        awaitEvent >>= \case
            Left Interrupt -> do
                cleanup current
                liftIO $ exitWith $ ExitFailure 1
            Right e | Just e == submitEvent current -> do
                cleanup current
                pure current
            Right e -> do
                let new = handleEvent e current
                go (Just current) new

runWidget :: forall m w. (MonadTerminal m, Widget w) => w -> m w
runWidget = runWidget' (const $ pure ()) (const flush)
