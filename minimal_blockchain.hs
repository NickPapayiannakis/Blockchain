module MinimalBlockchain where

data Transaction = Transaction { from  :: String
                               , to    :: String
                               , value :: Float } deriving Show

data Block = Block { version        :: String
                   , index          :: Int
                   , transactions   :: [Transaction]
                   , prevBlockHash  :: String
                   , merkleRootHash :: String
                   , time           :: ClockTime
                   , nonce          :: Integer } deriving Show

