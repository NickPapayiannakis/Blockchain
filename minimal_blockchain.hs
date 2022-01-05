module MinimalBlockchain where


import qualified Data.ByteString.Char8 as Char8
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Time.Clock.POSIX

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

--------------------
--Blockchain tools--
--------------------

addTransaction :: Block -> Transaction -> Block
addTransaction (Block v i txs ph mh t n) tx = Block v i (txs ++ [tx]) ph mh t n 

----------------
--Cryptography--
----------------

hashBlock :: Block -> String
hashBlock (Block v i txs ph mh t n) = digest
  where
    digest = Char8.unpack (SHA256.finalize ctx)
    ctx    = SHA256.updates ctx0 $ fmap Char8.pack [blockTxStr, ph]
    ctx0   = SHA256.init
    blockTxStr = foldr ((++) . show) "" txs