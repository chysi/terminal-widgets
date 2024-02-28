{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Render where

import Data.Text qualified as Text
import Prettyprinter
import Prelude

deriving stock instance Generic Position

type MonadCursor t m m' =
    ( MonadTrans t
    , MonadScreen m'
    , m ~ t m'
    , MonadState Position m
    )

render
    :: (MonadTerminal m)
    => Maybe (Position, Doc (Attribute m))
    -> (Position, Doc (Attribute m))
    -> m ()
render maybeOld (newPos, newDoc) = flip evalStateT (maybe Position{row = 0, col = 0} fst maybeOld) do
    moveToPosition Position{row = 0, col = 0}
    lift $ eraseInDisplay EraseForward
    lift $ putDoc newDoc
    moveToPosition newPos
  where
    moveToRow :: (MonadCursor t m m') => Int -> m ()
    moveToRow newRow = do
        oldRow <- gets (.row)
        lift $ case compare newRow oldRow of
            GT -> moveCursorDown $ newRow - oldRow
            LT -> moveCursorUp $ oldRow - newRow
            EQ -> pure ()
        modify $ #row .~ newRow

    moveToColumn :: (MonadCursor t m m') => Int -> m ()
    moveToColumn newCol = do
        oldCol <- gets (.col)
        when (oldCol /= newCol) do
            lift (setCursorColumn newCol)
            modify $ #col .~ newCol

    moveToPosition :: (MonadCursor t m m') => Position -> m ()
    moveToPosition Position{..} = moveToRow row >> moveToColumn col

    putLn :: (MonadCursor t m m') => m ()
    putLn = lift Prelude.putLn >> moveToColumn 0 >> modify (#row %~ succ)
    putText :: (MonadCursor t m m') => Text -> m ()
    putText t = lift (Prelude.putText t) >> modify (#col %~ (+ Text.length t))

renderLine :: (MonadScreen m) => Text -> Text -> m ()
renderLine oldText newText =
    unless (oldText == newText) do
        let (Text.length -> prefixLen, Text.length -> oldSuffixLen, newSuffix) =
                commonPrefixes oldText newText
        setCursorColumn prefixLen
        putText newSuffix
        when (oldSuffixLen > Text.length newSuffix) $ eraseInLine EraseForward
