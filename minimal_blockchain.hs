module MinimalBlockchain where


import qualified Data.ByteString.Char8 as Char8
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Time.Clock.POSIX
import Data.String   (fromString)

---------------
--Definitions--
---------------
data Transaction = Transaction { from  :: String
                               , to    :: String
                               , value :: Float } deriving Show

data Block = Block { version        :: Int
                   , index          :: Int
                   , transactions   :: [Transaction]
                   , prevBlockHash  :: String
                   , merkleRootHash :: String
                   , nonce          :: Integer } deriving Show

genesis :: Block
genesis = Block
    { version        = 1
    , index          = 0
    , transactions   = []
    , prevBlockHash  = "000000000000000000000000000000000"
    , merkleRootHash = "000000000000000000000000000000000"
    , nonce          = 0 }

--------------------
--Blockchain tools--
--------------------

addTransaction :: Block -> Transaction -> Block
addTransaction (Block v i txs ph mh n) tx = Block v i (txs ++ [tx]) ph mh n 

mine :: Block -> Integer -> Block
mine block@(Block v i txs ph _ _ ) n = case head pow of
                        '0' -> Block v (i + 1) txs ph hashOfBlock n
                        _   -> mine block (n + 1)
  where
    hashOfBlock = hashBlock block
    ctx         = SHA256.updates SHA256.init $ fmap fromString [hashOfBlock, ph, show n]
    pow         = Char8.unpack $ SHA256.finalize ctx

----------------
--Cryptography--
----------------

hashBlock :: Block -> String
hashBlock (Block v i txs ph mh n) = digest
  where
    digest     = Char8.unpack (SHA256.finalize ctx)
    ctx        = SHA256.updates SHA256.init $ map Char8.pack [blockTxStr, ph]
    blockTxStr = foldr ((++) . show) "" txs