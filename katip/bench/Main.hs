{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.DeepSeq
import           Criterion.Main
import           Data.Aeson
import           Data.Monoid
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.IO
import           System.Posix
-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Scribes.Handle
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [
    handleScribeBench
  ]


-------------------------------------------------------------------------------
handleScribeBench :: Benchmark
handleScribeBench = bgroup "Katip.Scribes.Handle" [
      env setupEnv $ \ ~(Scribe push, tid) ->
      bench "Bytestring Builder" $
        whnfIO $ push $ exItem tid
    ]
  where
    setupEnv = do
      scribe <- setup
      tid <- myThreadId
      return (scribe, mkThreadIdText tid)


-------------------------------------------------------------------------------
exItem :: ThreadIdText -> Item ExPayload
exItem tid = Item {
      _itemApp = Namespace ["app"]
    , _itemEnv = Environment "production"
    , _itemSeverity = WarningS
    , _itemThread = tid
    , _itemHost = "example"
    , _itemProcess = CPid 123
    , _itemPayload = ExPayload
    , _itemMessage = "message"
    , _itemTime = mkUTCTime 2015 3 14 1 5 9
    , _itemNamespace = Namespace ["foo"]
    , _itemLoc = Nothing
    }


-------------------------------------------------------------------------------
data ExPayload = ExPayload

instance ToJSON ExPayload where
  toJSON _ = Object mempty

instance ToObject ExPayload

instance LogItem ExPayload where
  payloadKeys _ _ = AllKeys


-------------------------------------------------------------------------------
mkUTCTime :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkUTCTime y mt d h mn s = UTCTime day dt
  where
    day = fromGregorian y mt d
    dt = h * 60 * 60 + mn * 60 + s


-------------------------------------------------------------------------------
setup :: IO Scribe
setup = do
  h <- openFile "/dev/null" WriteMode
  mkHandleScribe ColorIfTerminal h DebugS V0


-------------------------------------------------------------------------------
deriving instance NFData ThreadIdText


instance NFData Scribe where
  rnf (Scribe _) = ()
