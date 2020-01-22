{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsearch where

import Control.Monad (void)
import qualified Data.Aeson as Json
import Data.Default.Class (Default (def))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sortOn)
import qualified Data.Ord as Ord
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types (status400)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Servant ((:<|>) (..), (:>))
import qualified Servant
import qualified Servant.Checked.Exceptions as E
import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus (toErrStatus))
import Text.Casing (camel)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)

-- SERVER ---
serverMain :: [FunctionRecord] -> Int -> IO ()
serverMain fns port = do
  let settings =
        def {Log.outputFormat = Log.CustomOutputFormatWithDetails formatAsJSON}
  logger <- Log.mkRequestLogger settings
  Warp.run port $ (simpleCors . logger) $ app fns

app :: [FunctionRecord] -> Servant.Application
app = Servant.serve tsearchAPI . tsearchServer

type TsearchAPI = HelloHandler :<|> SearchHandler

tsearchAPI :: Servant.Proxy TsearchAPI
tsearchAPI = Servant.Proxy

tsearchServer :: [FunctionRecord] -> Servant.Server TsearchAPI
tsearchServer fns = helloHandler :<|> searchHandler fns

type HelloHandler = E.NoThrow :> Servant.Get '[Servant.JSON] Text

helloHandler :: Servant.Handler (E.Envelope '[] Text)
helloHandler = E.pureSuccEnvelope "Hello world"

type SearchHandler =
  "search"
    :> Servant.QueryParam "query" String
    :> E.Throws ResponseError
    :> Servant.Get '[Servant.JSON] [FunctionRecord]

type SearchHandler' = Servant.Handler (E.Envelope '[ResponseError] [FunctionRecord])

searchHandler :: [FunctionRecord] -> Maybe String -> SearchHandler'
searchHandler fns (Just q) =
  case P.parse query "" q of
    Right s -> E.pureSuccEnvelope $ find s fns
    Left e -> E.pureErrEnvelope $ InvalidQuery $ show e
searchHandler _ Nothing = E.pureErrEnvelope MissingQuery

data ResponseError
  = MissingQuery
  | InvalidQuery String
  deriving (Generic, Show, Json.ToJSON, Json.FromJSON)

instance ErrStatus ResponseError where
  toErrStatus _ = status400

-- Types ---

dropLabelPrefix :: Int -> Json.Options
dropLabelPrefix n =
  Json.defaultOptions {Json.fieldLabelModifier = camel . drop n}

data Param
  = Param
      { pName :: String,
        pType :: String,
        pIsGeneric :: Maybe Bool
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Param where
  toJSON = Json.genericToJSON $ dropLabelPrefix 1

instance Json.FromJSON Param where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 1

data Lines
  = Lines
      { lFrom :: Integer,
        lTo :: Integer
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Lines where
  toJSON = Json.genericToJSON $ dropLabelPrefix 1

instance Json.FromJSON Lines where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 1

data Location
  = Location
      { locPath :: String,
        locLines :: Lines
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Location where
  toJSON = Json.genericToJSON $ dropLabelPrefix 3

instance Json.FromJSON Location where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 3

data FunctionRecord
  = FunctionRecord
      { frName :: Maybe String,
        frDocs :: Maybe String,
        frText :: Maybe String,
        frParams :: [Param],
        frReturn :: String,
        frLocation :: Location,
        frModule :: String
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON FunctionRecord where
  toJSON = Json.genericToJSON $ dropLabelPrefix 2

instance Json.FromJSON FunctionRecord where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 2

data Module
  = Module
      { mName :: String,
        mFns :: [FunctionRecord]
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Module where
  toJSON = Json.genericToJSON $ dropLabelPrefix 1

instance Json.FromJSON Module where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 1

-- Query Parsing

data Signature
  = Signature {sigParams :: [String], sigReturn :: String}
  deriving (Show, Eq)

data Query
  = ByName String
  | BySignature Signature
  deriving (Show, Eq)

query :: Parser Query
query = P.try byName <|> bySignature

byName :: Parser Query
byName = ByName <$> (whitespace *> lexeme varName <* P.eof)

bySignature :: Parser Query
bySignature =
  BySignature
    <$> ( Signature
            <$> params <* lexeme (C.string "=>")
            <*> lexeme (P.many1 $ C.noneOf "\n\t") <* P.eof
        )

params :: Parser [String]
params = P.sepBy (lexeme (P.many1 $ C.noneOf "=,\n\t")) (lexeme $ P.char ',')

varName :: Parser String
varName = lexeme ((:) <$> firstChar <*> P.many nonFirstChar)
  where
    firstChar = C.letter <|> C.char '_'
    nonFirstChar = C.digit <|> firstChar

whitespace :: Parser ()
whitespace = void $ P.many $ C.oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

-- Search

find :: Query -> [FunctionRecord] -> [FunctionRecord]
find (ByName name) fns =
  take 100
    $ map fst
    $ sortOn snd
    $ filter ((/= MatchesNothing) . snd)
    $ map (nameDistance name) fns
find (BySignature sig) fns =
  take 100
    $ map fst
    $ sortOn (Ord.Down . snd)
    $ filter ((> 0) . snd)
    $ map (weighFunctionRecord sig) fns

data NameMatch
  = MatchesFull
  | MatchesPrefix
  | MatchesSuffix
  | MatchesInfix
  | MatchesNothing
  deriving (Show, Eq, Ord)

nameDistance :: String -> FunctionRecord -> (FunctionRecord, NameMatch)
nameDistance _ fn@(FunctionRecord Nothing _ _ _ _ _ _) = (fn, MatchesNothing)
nameDistance queryName fn@(FunctionRecord (Just fnName) _ _ _ _ _ _)
  | queryName == fnName = (fn, MatchesFull)
  | queryName `isPrefixOf` fnName = (fn, MatchesPrefix)
  | queryName `isSuffixOf` fnName = (fn, MatchesSuffix)
  | queryName `isInfixOf` fnName = (fn, MatchesInfix)
  | otherwise = (fn, MatchesNothing)

-- ¯\_(ツ)_/¯
weighFunctionRecord :: Signature -> FunctionRecord -> (FunctionRecord, Float)
weighFunctionRecord q fn = (fn, returnWeight + paramsWeight)
  where
    returnWeight = if sigReturn q == frReturn fn then 2 else 0
    paramsWeight = weighParams (sigParams q) (pType <$> frParams fn)
    weighParams :: [String] -> [String] -> Float
    weighParams qParams fnParams
      | length qParams == length fnParams =
        let both = zip qParams fnParams
            matches = map (\(a, b) -> if a == b then 1 else 0) both
            w = sum matches / realToFrac (length qParams)
         in if w > 0 then w + 1 else w
      | otherwise =
        let matches = map (\qp -> if qp `elem` fnParams then 1 else 0) qParams
         in sum matches / realToFrac (length qParams)
