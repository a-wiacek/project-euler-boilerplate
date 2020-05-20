module Utils.Monad
    ( ifteM
    , whenM
    , unlessM
    ) where
import Control.Monad

ifteM :: Monad m => m Bool -> m a -> m a -> m a
ifteM cond t f = cond >>= \b -> if b then t else f

whenM :: Monad m => m Bool -> m () -> m () 
whenM cond f = cond >>= \b -> when b f

unlessM :: Monad m => m Bool -> m () -> m () 
unlessM cond f = cond >>= \b -> unless b f