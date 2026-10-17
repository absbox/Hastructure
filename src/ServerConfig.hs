{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Server-side request bounding for the multi-run HTTP endpoints (#345).
--
-- The engine accepts scenario\/deal maps and cartesian grids whose size is
-- chosen by the client; unbounded, these are a denial-of-service vector
-- (memory and CPU). Each multi-run endpoint has its own total-run cap
-- ('RunLimits', mirrored 1:1 by the @maxRunNum.\<endpoint\>@ keys in
-- @config.yml@) and its in-flight evaluation is bounded by a worker cap
-- resolved from config \/ environment \/ RTS capabilities
-- ('resolveWorkerNum').
module ServerConfig
    ( defaultRunNum
    , resolveRunNum
    , RunLimitsRaw(..)
    , emptyRunLimitsRaw
    , RunLimits(..)
    , resolveRunLimits
    , checkRunNumLimit
    , resolveWorkerNum
    , resolveWorkerCap
    , badRequestJSON
    )
    where

import Data.Aeson (FromJSON(..), withObject, (.:?), encode, Value)
import GHC.Generics (Generic)
import Network.HTTP.Types (hContentType)
import Servant.Server (ServerError(..), err400)
import Text.Read (readMaybe)

-- | A 400 rejection rendered in the standard JSON error envelope used by the
-- in-band 'Either' errors and the #344 exception boundary: @{"Left":"…"}@ with
-- an @application\/json@ content type (#322 review M1).
badRequestJSON :: String -> ServerError
badRequestJSON msg = err400
  { errHeaders = [(hContentType, "application/json")]
  , errBody = encode (Left msg :: Either String Value)
  }

-- | Default total-run cap per multi-run request.
defaultRunNum :: Int
defaultRunNum = 1000

-- | Resolve a single @maxRunNum@ value: an absent key takes 'defaultRunNum',
-- a present value must be positive.
resolveRunNum :: Maybe Int -> Either String Int
resolveRunNum Nothing = Right defaultRunNum
resolveRunNum (Just n)
  | n > 0     = Right n
  | otherwise = Left ("invalid value " ++ show n ++ ": must be a positive integer")

-- | The raw @maxRunNum@ block as parsed from @config.yml@; every key is
-- optional. JSON keys mirror the HTTP endpoint names.
data RunLimitsRaw = RunLimitsRaw
  { rawRunPoolByScenarios    :: Maybe Int
  , rawRunDealByScenarios    :: Maybe Int
  , rawRunMultiDeals         :: Maybe Int
  , rawRunDealByRunScenarios :: Maybe Int
  , rawRunByCombo            :: Maybe Int
  } deriving (Show,Generic)

-- | A @maxRunNum@ block entirely absent from @config.yml@.
emptyRunLimitsRaw :: RunLimitsRaw
emptyRunLimitsRaw = RunLimitsRaw Nothing Nothing Nothing Nothing Nothing

instance FromJSON RunLimitsRaw where
  parseJSON = withObject "maxRunNum" $ \v -> RunLimitsRaw
    <$> v .:? "runPoolByScenarios"
    <*> v .:? "runDealByScenarios"
    <*> v .:? "runMultiDeals"
    <*> v .:? "runDealByRunScenarios"
    <*> v .:? "runByCombo"

-- | Resolved per-endpoint total-run caps. Field names mirror the HTTP
-- endpoints; the @config.yml@ keys are @maxRunNum.\<endpoint\>@.
data RunLimits = RunLimits
  { maxRunPoolByScenarios    :: Int
  , maxRunDealByScenarios    :: Int
  , maxRunMultiDeals         :: Int
  , maxRunDealByRunScenarios :: Int
  , maxRunByCombo            :: Int
  } deriving (Show,Eq)

-- | Fill the per-endpoint defaults and validate every cap, naming the
-- offending key in the error.
resolveRunLimits :: RunLimitsRaw -> Either String RunLimits
resolveRunLimits (RunLimitsRaw a b c d e)
  = RunLimits <$> key "runPoolByScenarios" a
              <*> key "runDealByScenarios" b
              <*> key "runMultiDeals" c
              <*> key "runDealByRunScenarios" d
              <*> key "runByCombo" e
  where
    key nm mv = case resolveRunNum mv of
                  Right n  -> Right n
                  Left msg -> Left ("maxRunNum." ++ nm ++ ": " ++ msg)

-- | Reject a multi-run request whose total run count exceeds the endpoint
-- cap. @ep@ is the endpoint name; it is both the subject of the message and
-- the @maxRunNum.\<ep\>@ config key path quoted in it.
checkRunNumLimit :: String -> Int -> Int -> Either String ()
checkRunNumLimit ep lim n
  | n <= lim  = Right ()
  | otherwise =
      let key = "maxRunNum." ++ ep
      in Left ( ep ++ " would run " ++ show n ++ " runs, exceeding " ++ key ++ " " ++ show lim
                ++ " (config.yml); reduce the request size or raise " ++ key
                ++ " in config.yml and restart the engine" )

-- | Resolve the in-flight worker cap for the multi-run endpoints. Precedence:
-- the @HASTRUCTURE_WORKERS@ environment variable when it parses to a positive
-- 'Int' (kept for container deployments that set it today), then the
-- @workerNum@ key in @config.yml@, then the RTS capability count (@-N@).
-- A present-but-invalid value at either layer falls back to the next one —
-- workers is a performance knob, unlike 'RunLimits' which fails fast.
resolveWorkerNum :: Int -> Maybe Int -> Maybe String -> Int
resolveWorkerNum caps mCfg mEnv
  | Just n <- mEnv >>= readMaybe, n > 0 = n
  | Just n <- mCfg, n > 0               = n
  | otherwise                           = caps

-- | Two-layer special case of 'resolveWorkerNum' (RTS caps \/ environment),
-- preserved for existing callers and tests.
resolveWorkerCap :: Int -> Maybe String -> Int
resolveWorkerCap caps mEnv = resolveWorkerNum caps Nothing mEnv
