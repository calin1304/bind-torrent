{-# LANGUAGE TemplateHaskell #-}

module MovingWindow
       ( MovingWindow
       , new
       , insert
       , get
       , clear
       ) where

import           Control.Lens          (folded, makeLenses, sumOf, (%~), (^.),
                                        (^?), _1, _2, _head, _last)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Time.Clock.POSIX

data MovingWindow = MovingWindow
    { _values :: [(POSIXTime, Int)]
    , _size   :: Int
    } deriving (Show)
makeLenses ''MovingWindow

new :: Int -> MovingWindow
new = MovingWindow []

insert :: (POSIXTime, Int) -> MovingWindow -> MovingWindow
insert x w = w & values %~ (x:) . take ((w ^. size) - 1)

get :: MovingWindow -> Maybe Int
get w =
    case w ^. values of
        []  -> Nothing
        [x] -> x ^? _2
        _   -> round . (total /) <$> diff
                where tinit = w ^? values . _last . _1
                      tfin  = w ^? values . _head . _1
                      diff  = (-) <$> tfin <*> tinit <&> toRational
                      total = w & sumOf (values . folded . _2) & toRational

clear :: MovingWindow -> MovingWindow
clear w = new $ w ^. size
