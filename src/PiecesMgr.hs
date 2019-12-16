{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PiecesMgr
       ( newEnvFromMeta
       , start
       , cleanup
       )
       where

import           Control.Concurrent.STM.TBChan
import           Control.Monad.Reader
import           Control.Monad.STM
import           Data.Torrent
import           Debug.Trace
import           System.IO

import           Data.Function                ((&))

import           InternalMessage

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy.Char8   as LC

type PiecesMgrM a = ReaderT PiecesMgrEnv IO a

data PiecesMgrEnv = PiecesMgrEnv
    { pmTorrent    :: Torrent
    , pmInbox      :: TBChan PiecesMgrMessage
    , pmFileHandle :: Handle
    }

handleMessage :: PiecesMgrMessage -> PiecesMgrM ()
handleMessage = \case
    HavePiece ix bs -> writePiece ix bs        

writePiece :: Int -> BS.ByteString -> PiecesMgrM ()
writePiece ix bs = do
    -- FIXME: Check SHA1 before writing
    off <- (* fromIntegral ix) <$> asks (tPieceLength . tInfo . pmTorrent)
    handle <- asks pmFileHandle  -- liftIO $ openFile fname ReadWriteMode
    "Writing piece " ++ show ix ++ " at offset " ++ show off & logPiecesMgr
    liftIO $ do
        hSeek handle AbsoluteSeek off
        BS.hPut handle bs

newEnvFromMeta :: Torrent -> TBChan PiecesMgrMessage -> IO PiecesMgrEnv
newEnvFromMeta torrent fromPeers = do
    let fname = torrent & LC.unpack . tName . tInfo
    handle <- openFile fname ReadWriteMode
    case tInfo torrent of
        MultiFile{}  -> error "Multifile torrent not supported"
        SingleFile{} -> return $ PiecesMgrEnv torrent fromPeers handle

start :: PiecesMgrEnv -> IO ()
start = runReaderT listenerLoop
    where 
        listenerLoop = forever $ 
            asks pmInbox >>= liftIO . atomically . readTBChan >>= handleMessage 

cleanup :: PiecesMgrEnv -> IO ()
cleanup env = do
    "Cleanup" & logPiecesMgr
    hClose (pmFileHandle env)

logPiecesMgr :: (Applicative m) => String -> m ()
logPiecesMgr s = traceM $ "PiecesMgr :: " ++ s
