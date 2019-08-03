module MovingWindow
       ( MovingWindow
       , new
       , insert
       , get
       , clear
       ) where

import           Data.Time.Clock.POSIX

data MovingWindow = MovingWindow
    { mwData       :: [(POSIXTime, Int)]
    , mwWindowSize :: Int
    , mwLength     :: Int
    } deriving (Show)

new :: Int -> MovingWindow
new windowSize = MovingWindow [] windowSize 0

insert :: MovingWindow -> (POSIXTime, Int) -> MovingWindow
insert mw x =
    if mwWindowSize mw == mwLength mw
        then mw { mwData = x : init (mwData mw) }
        else mw { mwData = x : mwData mw, mwLength = mwLength mw + 1 }

get :: MovingWindow -> Int
get mw =
    case mwLength mw of
        0 -> 0
        1 -> snd $ head $ mwData mw
        _ -> round $ total / diff
                where tinit = fst $ last $ mwData mw
                      tfin  = fst $ head $ mwData mw
                      diff  = toRational (tfin - tinit)
                      total = sum $ map (fromIntegral . snd) $ mwData mw :: Rational

clear :: MovingWindow -> MovingWindow
clear mw = new (mwWindowSize mw)
