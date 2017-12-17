{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


import qualified Network.Wreq as W
import Control.Lens
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Control.Exception (catch, SomeException)
import Control.Concurrent.Async (async, waitBoth)
import System.Environment (getArgs, getProgName)


data Currency = Currency {
    id :: String
  , name :: String
  , symbol :: String
  , price_usd :: String
  , price_btc :: String
} deriving (Show, Generic)

instance FromJSON Currency


getCurrencies :: IO (Maybe (String, String))
getCurrencies = do
    args <- getArgs
    case args of
        (currID1:currID2:_) -> return $ Just (currID1, currID2)
        otherwise -> return Nothing


printUsage :: IO ()
printUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <currency-ID-#1> <currency-ID-#2>"
    putStrLn $ "for example: " ++ progName ++ " bitcoin ethereum"


getCurrencyInfo :: String -> IO (Either String Currency)
getCurrencyInfo currency_id = do
    let uri = "https://api.coinmarketcap.com/v1/ticker/" ++ currency_id ++ "/"
    catch (fmap Right $ get uri)
        (\e -> do
            let err = show (e :: SomeException)
            return $ Left err)
    where get u = do
            r <- W.get u >>= W.asJSON
            return $ head (r ^. W.responseBody)


currencyRatio :: Currency -> Currency -> (Double, Double)
currencyRatio c1 c2 = (by_usd, by_btc)
    where
        by_usd = read (price_usd c1) / read (price_usd c2)
        by_btc = read (price_btc c1) / read (price_btc c2)


mkComparison :: String -> String -> IO ()
mkComparison cid1 cid2 = do
    a1 <- async $ getCurrencyInfo cid1
    a2 <- async $ getCurrencyInfo cid2
    result <- waitBoth a1 a2

    case result of
        (Left e1, _) -> putStrLn $ "Failed to get info for: " ++ cid1
        (_, Left e2) -> putStrLn $ "Failed to get info for: " ++ cid2
        (Right c1, Right c2) -> do
            let (by_usd, by_btc) = currencyRatio c1 c2
            putStrLn $ "by USD price: 1 " ++ symbol c1 ++ " = " ++ show by_usd ++ " " ++ symbol c2
            putStrLn $ "by BTC price: 1 " ++ symbol c1 ++ " = " ++ show by_btc ++ " " ++ symbol c2


main :: IO ()
main = do
    currencies <- getCurrencies
    case currencies of
        Just (cid1, cid2) -> mkComparison cid1 cid2
        Nothing -> printUsage
