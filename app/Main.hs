{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import qualified System.Environment as Env
import Text.Layout.Table
import Text.Printf
import Universum

main :: IO ()
main =
  handleArgs
    >>= \case
      Left err ->
        putStrLn $ "Error processing: " <> err
      Right cslug ->
        getNftCollectionStats cslug
          >>= \case
            Right stats ->
              case stats of
                Just a ->
                  let title = "Here are the stats for: \"" <> (unNftCollectionSlug cslug) <> "\" collection:"
                      table = tableString
                        [def, fixedLeftCol 20]
                        unicodeRoundS
                        def
                        [ rowG ["Average price" :: String, (formatFloat . averagePrice $ a) <> " ETH" :: String],
                          rowG ["Floor price" :: String, (formatFloat . floorPrice $ a) <> " ETH" :: String],
                          rowG ["Total volume" :: String, (formatFloat . totalVolume $ a) <> " ETH" :: String],
                          rowG ["Total sales" :: String, (show . totalSales $ a) :: String],
                          rowG ["Total items" :: String, (show . totalSupply $ a) :: String],
                          rowG ["Total owners" :: String, (show . numOwners $ a) :: String]
                        ]
                  in putStrLn title >> putStrLn table
                Nothing ->
                  let errMsg = "Failed to decode collection stats" :: String
                   in putStrLn errMsg
            Left err -> putStrLn . unApiError $ err
  where
    formatFloat :: Float -> String
    formatFloat =
      printf "%.2f"

handleArgs :: IO (Either String NftCollectionSlug)
handleArgs =
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [cslug] -> Right . NftCollectionSlug $ cslug
        [] -> Left "no collection name provided"
        _ -> Left "too many arguments"

apiHost :: BC.ByteString
apiHost = "api.opensea.io"

apiToken :: BC.ByteString
apiToken = "TODO"

getNftCollectionStats :: NftCollectionSlug -> IO (Either ApiError (Maybe NftCollectionStats))
getNftCollectionStats collectionSlug =
  let apiPath = BC.pack $ "/api/v1/collection/" <> (unNftCollectionSlug collectionSlug) <> "/stats"
      apiMethod = "GET"
      req = buildApiRequest apiToken apiHost apiMethod apiPath
      resp = mkApiCall $ req
   in resp >>= pure . second (decode . LC.pack . unApiSuccess)

mkApiCall :: Request -> IO (Either ApiError ApiSuccess)
mkApiCall req = do
  resp <- httpLBS req
  let callResult = case getResponseStatusCode resp of
        200 -> Right . handleApiCallSuccess $ resp
        otherwise -> Left . handleApiCallError $ resp
  pure callResult

handleApiCallSuccess :: Response LC.ByteString -> ApiSuccess
handleApiCallSuccess resp =
  ApiSuccess . LC.unpack . getResponseBody $ resp

handleApiCallError :: Response LC.ByteString -> ApiError
handleApiCallError resp =
  ApiError $ "API error:" <> (LC.unpack . getResponseBody $ resp)

buildApiRequest ::
  BC.ByteString ->
  BC.ByteString ->
  BC.ByteString ->
  BC.ByteString ->
  Request
buildApiRequest token host method path =
  setRequestMethod method $
    setRequestHost host $
      -- setRequestHeader "token" [token] $
      setRequestPath path $
        setRequestSecure True $
          setRequestPort 443 $
            defaultRequest

newtype ApiError = ApiError {unApiError :: String}

newtype ApiSuccess = ApiSuccess {unApiSuccess :: String}

newtype NftCollectionSlug = NftCollectionSlug {unNftCollectionSlug :: String}

data NftCollectionStats = NftCollectionStats
  { averagePrice :: Float,
    floorPrice :: Float,
    numOwners :: Natural,
    totalVolume :: Float,
    totalSales :: Natural,
    totalSupply :: Natural
  }
  deriving (Show)

(.->) :: FromJSON a => Parser Object -> Data.Aeson.Types.Key -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

instance FromJSON NftCollectionStats where
  parseJSON = withObject "NftCollectionStats" $ \obj -> do
    averagePrice <- obj .: "stats" .-> "average_price"
    floorPrice <- obj .: "stats" .-> "floor_price"
    numOwners <- obj .: "stats" .-> "num_owners"
    totalVolume <- obj .: "stats" .-> "total_volume"
    totalSales <- obj .: "stats" .-> "total_sales"
    totalSupply <- obj .: "stats" .-> "total_supply"
    return
      ( NftCollectionStats
          { averagePrice = averagePrice,
            floorPrice = floorPrice,
            numOwners = numOwners,
            totalVolume = totalVolume,
            totalSupply = totalSupply,
            totalSales = totalSales
          }
      )
