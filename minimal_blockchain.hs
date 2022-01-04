module MinimalBlockchain where

import Data.Time.Clock.POSIX

data Transaction = Transaction { from  :: String
                               , to    :: String
                               , value :: Float } deriving Show

data Block = Block { version        :: Int
                   , index          :: Int
                   , transactions   :: [Transaction]
                   , prevBlockHash  :: String
                   , merkleRootHash :: String
                   , time           :: POSIXTime
                   , nonce          :: Maybe Integer } deriving Show

genesis :: IO Block
genesis = do
    let version = 1
    let index = 0
    let transactions = []
    let prevBlockHash = "000000000000000000000000000000000"
    let merkleRootHash = "000000000000000000000000000000000"
    time <- getPOSIXTime
    let nonce = Nothing
    return (Block version index transactions prevBlockHash merkleRootHash time nonce)

addTransaction :: Block -> Transaction -> Block
addTransaction (Block v i txs ph mh t n) tx = Block v i (txs ++ [tx]) ph mh t n 

hashBlock :: Block -> String
hashBlock = undefined