module Utils.Memo
    ( StrictMemo
    , module Control.Monad.Memo
    ) where
import Control.Monad.Memo
import Data.Functor.Identity
import qualified Data.Map.Strict as Map

-- Slight modification of Control.Monad.Memo to use strict version of Map
type StrictMemo k v = MemoStateT (Map.Map k v) k v Identity