module MinimalBlockchain where

import Data.Time.Clock.POSIX

data Transaction = Transaction { from  :: String
                               , to    :: String
                               , value :: Float 
                               } deriving Show

data Block = Block { version        :: Int
                   , index          :: Int
                   , transactions   :: [Transaction]
                   , prevBlockHash  :: String
                   , merkleRootHash :: String
                   , time           :: POSIXTime
                   , nonce          :: Maybe Integer 
                   } deriving Show

genesis :: IO Block
genesis = do
    let version = 1
    let  index = 0
    let  transactions = []
    let  prevBlockHash = "000000000000000000000000000000000"
    let  merkleRootHash = ""
    time <- getPOSIXTime
    let  nonce = Nothing
    return (Block version index transactions prevBlockHash merkleRootHash time Nothing)

addTransaction :: Transaction -> [Transaction]
addTransaction = undefined

hashBlock :: Block -> String
hashBlock = undefined