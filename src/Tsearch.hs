{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsearch where

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import Data.Default.Class (Default (def))
import Data.Foldable (fold)
import Data.List (dropWhileEnd, intersperse, isInfixOf, isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types (status400)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import qualified Network.Wreq as Wreq
import Servant ((:<|>) (..), (:>))
import qualified Servant
import qualified Servant.Checked.Exceptions as E
import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus (toErrStatus))
import Text.Casing (camel)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import qualified Text.ParserCombinators.Parsec.Number as N

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
    Right query' -> do
      let result = find query' fns
      when False $ void . liftIO . forkIO $ publishToAnalytics query' result
      E.pureSuccEnvelope result
    Left e -> E.pureErrEnvelope $ InvalidQuery $ show e
searchHandler _ Nothing = E.pureErrEnvelope MissingQuery

publishToAnalytics :: Query -> [FunctionRecord] -> IO ()
publishToAnalytics q fns = do
  let payload = AnalyticsPayload (show q) (take 5 fns) (length fns)
  -- TODO: Firebase URL
  r <- Wreq.post "http://localhost:9000" (Json.toJSON payload)
  let status = r ^. Wreq.responseStatus . Wreq.statusCode
  putStrLn "Published Analytics"
  putStrLn $ "Status: " ++ show status
  putStrLn $ "Query: " ++ show q
  putStrLn $ show (length fns) ++ " results"

data AnalyticsPayload
  = AnalyticsPayload
      { apQuery :: String,
        apResult :: [FunctionRecord],
        apCount :: Int
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON AnalyticsPayload where
  toJSON = Json.genericToJSON $ dropLabelPrefix 2

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
        frLocation :: Location,
        frModule :: String,
        frSignature :: Signature
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

data Signature
  = Signature
      { sigtParameters :: [Param],
        sigtReturnType :: Type
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Signature where
  toJSON = Json.genericToJSON $ dropLabelPrefix 4

instance Json.FromJSON Signature where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 4

data Param
  = Param
      { paramName :: String,
        paramType :: Type
      }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Param where
  toJSON = Json.genericToJSON $ dropLabelPrefix 5

instance Json.FromJSON Param where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 5

data Type
  = Any
  | Unknown
  | Null
  | Undefined
  | Void
  | Never
  | BoolT
  | StringT
  | NumberT
  | LiteralString String
  | LiteralNumber Float
  | LiteralBool Bool
  | Union [Type] -- !!!
  | Fn [Type] Type
  | Generic Int Int -- !!!
  | ArrayT Type
  | HigherOrder1 String Type
  | HigherOrderN String [Type]
  | Named String
  | Other String
  deriving (Generic, Show, Eq)

showType :: Type -> String
showType Any = "any"
showType Unknown = "unknown"
showType Undefined = "undefined"
showType Null = "null"
showType Void = "void"
showType Never = "never"
showType BoolT = "boolean"
showType StringT = "string"
showType NumberT = "number"
showType (LiteralString str) = "'" ++ str ++ "'"
showType (LiteralNumber n) = show n
showType (LiteralBool True) = "true"
showType (LiteralBool False) = "false"
showType (Union ts) = listJoin " | " $ fmap showType ts
showType (Fn args ret) = "(" ++ ps ++ ") => " ++ showType ret
  where
    param (t, c) = [c] ++ ": " ++ showType t
    ps = listJoin ", " (param <$> zip args ['a' ..])
showType (Generic 0 i) = [['A' ..] !! i]
showType (Generic lvl i) = ['A' ..] !! i : show lvl
showType (ArrayT t) = "Array<" ++ showType t ++ ">"
showType (HigherOrder1 name t) = name ++ "<" ++ showType t ++ ">"
showType (HigherOrderN name ts) =
  name ++ "<" ++ listJoin ", " (fmap showType ts) ++ ">"
showType (Named name) = name
showType (Other str) = str

instance Json.ToJSON Type where
  toJSON = Json.genericToJSON typeOptions

instance Json.FromJSON Type where
  parseJSON = Json.genericParseJSON typeOptions

sumEncoding :: Json.SumEncoding
sumEncoding =
  Json.TaggedObject
    { Json.tagFieldName = "__tag",
      Json.contentsFieldName = "values"
    }

typeOptions :: Json.Options
typeOptions =
  Json.defaultOptions
    { Json.tagSingleConstructors = True,
      Json.sumEncoding = sumEncoding
    }

-- Query Parsing

data Query
  = ByName String
  | BySignature Signature
  deriving (Show, Eq)

showQuery :: Query -> String
showQuery (ByName name) = name
showQuery (BySignature (Signature [] ret)) = "() => " ++ show ret
showQuery (BySignature (Signature ps ret)) =
  listJoin ", " (fmap (showType . paramType) ps) ++ " => " ++ show ret

query :: Parser Query
query = P.try byName <|> bySignature

byName :: Parser Query
byName = ByName <$> (whitespace *> lexeme varName <* P.eof)

bySignature :: Parser Query
bySignature =
  BySignature
    <$> ( Signature
            <$> (nameParams <$> lexeme params) <* lexeme (C.string "=>")
            <*> lexeme type_ <* P.eof
        )

nameParams :: [Type] -> [Param]
nameParams ts = addName <$> zip ts ['a' ..]
  where
    addName (t, c) = Param [c] t

params :: Parser [Type]
params =
  P.sepBy (lexeme type_) (lexeme $ P.char ',')

type_ :: Parser Type
type_ =
  P.try (Any <$ keyword "any")
    <|> P.try (Unknown <$ keyword "unknown")
    <|> P.try (Undefined <$ keyword "undefined")
    <|> P.try (Null <$ keyword "null")
    <|> P.try (Void <$ keyword "void")
    <|> P.try (Never <$ keyword "never")
    <|> P.try (BoolT <$ keyword "boolean")
    <|> P.try (StringT <$ keyword "string")
    <|> P.try (NumberT <$ keyword "number")
    <|> P.try (LiteralBool True <$ keyword "true")
    <|> P.try (LiteralBool False <$ keyword "false")
    <|> P.try (LiteralNumber <$> N.floating <* P.notFollowedBy C.alphaNum)
    <|> P.try (LiteralString <$> between (C.char '"') (P.many $ C.noneOf "\"\n"))
    <|> P.try (LiteralString <$> between (C.char '\'') (P.many $ C.noneOf "'\n"))
    <|> (Other . stripEnd <$> P.many1 (C.noneOf "=,\n\t"))

varName :: Parser String
varName = lexeme ((:) <$> firstChar <*> P.many nonFirstChar)
  where
    firstChar = C.letter <|> C.char '_'
    nonFirstChar = C.digit <|> firstChar

whitespace :: Parser ()
whitespace = void $ P.many $ C.oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

between :: Parser a -> Parser b -> Parser b
between p = P.between p p

keyword :: String -> Parser String
keyword str = C.string str <* P.notFollowedBy C.alphaNum

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
    $ filter (isJust . snd)
    $ sortOn snd
    $ fmap (signatureCost sig . fst)
    $ filter snd
    $ fmap (sig ~?) fns

data NameMatch
  = MatchesFull
  | MatchesPrefix
  | MatchesSuffix
  | MatchesInfix
  | MatchesNothing
  deriving (Show, Eq, Ord)

nameDistance :: String -> FunctionRecord -> (FunctionRecord, NameMatch)
nameDistance _ fn@(FunctionRecord Nothing _ _ _ _ _) = (fn, MatchesNothing)
nameDistance queryName fn@(FunctionRecord (Just fnName) _ _ _ _ _)
  | queryName == fnName = (fn, MatchesFull)
  | queryName `isPrefixOf` fnName = (fn, MatchesPrefix)
  | queryName `isSuffixOf` fnName = (fn, MatchesSuffix)
  | queryName `isInfixOf` fnName = (fn, MatchesInfix)
  | otherwise = (fn, MatchesNothing)

-- ¯\_(ツ)_/¯
signatureCost :: Signature -> FunctionRecord -> (FunctionRecord, Maybe Float)
signatureCost q fn@(FunctionRecord _ _ _ _ _ b) =
  (fn, sigtReturnType q ?? sigtReturnType b)

(??) :: Type -> Type -> Maybe Float
Any ?? Any = Just 0
Any ?? _ = Just 0.1
_ ?? Any = Nothing
Unknown ?? Unknown = Just 0
Unknown ?? _ = Just 0.1
_ ?? Unknown = Nothing
Null ?? Null = Just 0
Undefined ?? Null = Just 0.1
Null ?? Undefined = Just 0.1
Null ?? _ = Nothing
Void ?? Void = Just 0
Void ?? Undefined = Just 0.1
Undefined ?? Void = Just 0.1
Never ?? Never = Just 0
Never ?? _ = Nothing
_ ?? Never = Nothing
Void ?? _ = Nothing
_ ?? Void = Nothing
Undefined ?? _ = Nothing
_ ?? Undefined = Nothing
(LiteralString a) ?? (LiteralString b)
  | a == b = Just 0
  | otherwise = Nothing
(LiteralString _) ?? StringT = Just 0.8
StringT ?? (LiteralString _) = Just 0.8
(LiteralNumber a) ?? (LiteralNumber b)
  | a == b = Just 0
  | otherwise = Nothing
(LiteralNumber _) ?? NumberT = Just 0.8
NumberT ?? (LiteralNumber _) = Just 0.8
(LiteralBool a) ?? (LiteralBool b)
  | a == b = Just 0
  | otherwise = Nothing
(LiteralBool _) ?? BoolT = Just 0.3
BoolT ?? (LiteralBool _) = Just 0.3
BoolT ?? BoolT = Just 0
StringT ?? StringT = Just 0
NumberT ?? NumberT = Just 0
(Other a) ?? (Other b)
  | a == b = Just 0
  | otherwise = Nothing
a ?? b
  | a == b = Just 0
  | otherwise = Nothing

(~?) :: Signature -> FunctionRecord -> (FunctionRecord, Bool)
(~?) q fn = (fn, inDelta)
  where
    inDelta = sigtParameters q >< sigtParameters (frSignature fn)

-- Utils
stripEnd :: String -> String
stripEnd = dropWhileEnd (== ' ')

delta :: Int -> [a] -> [b] -> Bool
delta diff as bs = abs (length as - length bs) <= diff

(><) :: [a] -> [b] -> Bool
(><) = delta 1

listJoin :: [a] -> [[a]] -> [a]
listJoin what = fold . intersperse what

-- FIXTURES
fixtures :: [Type]
fixtures =
  -- types
  [ Any,
    Unknown,
    Null,
    Undefined,
    Void,
    Never,
    BoolT,
    StringT,
    NumberT,
    LiteralString "foo",
    LiteralNumber 123,
    LiteralBool True,
    Union [StringT, NumberT],
    Fn [] Void,
    HigherOrder1 "Promise" StringT,
    HigherOrderN "Either" [Named "Error", StringT],
    Generic 0 0,
    Generic 0 1,
    Generic 2 1,
    Named "IUser",
    Other "Promise<any>",
    -- Functions: string & number
    Fn [StringT, StringT] StringT,
    Fn [StringT, LiteralString "foo"] StringT,
    Fn [StringT, NumberT] StringT,
    Fn [StringT, LiteralNumber 10] StringT,
    Fn [Named "Function"] StringT,
    -- NumberT -> [a] -> [a]
    Fn [NumberT] $ Fn [ArrayT (Generic 1 0)] (ArrayT $ Generic 1 0),
    -- NumberT -> [a] -> [[a]]
    Fn [NumberT] $ Fn [ArrayT (Generic 1 0)] (ArrayT $ ArrayT $ Generic 1 0),
    -- Functions: any, never, void, unknown, undefined & null
    Fn [Any] Any,
    Fn [Any, Any] Any,
    Fn [Any] StringT,
    Fn [StringT] Any,
    Fn [StringT, StringT] Any,
    Fn [NumberT] Any,
    Fn [] Undefined,
    Fn [NumberT] Undefined,
    Fn [StringT] Undefined,
    Fn [] Null,
    Fn [] Void,
    Fn [StringT] Never,
    Fn [ArrayT StringT] Void,
    Fn [Never] Void,
    Fn [Never] (Generic 0 0),
    Fn [Unknown] (Named "Error"),
    -- Generics
    -- a -> a
    Fn [Generic 0 0] (Generic 0 0),
    -- (a, a) -> Bool
    Fn [Generic 0 0, Generic 0 0] BoolT,
    -- (a, b) -> b
    Fn [Generic 0 0, Generic 0 1] (Generic 0 1),
    -- [a] -> Bool
    Fn [ArrayT (Generic 0 0)] BoolT,
    -- (a, Any) -> Any
    Fn [Generic 0 0, Any] Any,
    -- Any -> a
    Fn [Any] (Generic 0 0),
    -- Any -> Promise<a>
    Fn [Any] (HigherOrder1 "Promise" $ Generic 0 0),
    -- Map<k, a> -> Bool
    Fn [HigherOrderN "Map" [Generic 0 0, Generic 0 1]] BoolT,
    -- Map<k, a> -> NumberT
    Fn [HigherOrderN "Map" [Generic 0 0, Generic 0 1]] NumberT,
    -- ([a], [b], ((a, b) -> c)) -> [c]
    Fn
      [ ArrayT (Generic 0 0),
        ArrayT (Generic 0 1),
        Fn [Generic 0 0, Generic 0 1] (Generic 0 2)
      ]
      (ArrayT $ Generic 0 2),
    -- ([a], [a]) -> a[]
    Fn [ArrayT (Generic 0 0), ArrayT (Generic 0 0)] (ArrayT $ Generic 0 0),
    -- (NumberT, a, [a]) -> [a]
    Fn [NumberT, Generic 0 0, ArrayT (Generic 0 0)] (ArrayT $ Generic 0 0),
    -- (NumberT, [a]) -> [a]
    Fn [NumberT, ArrayT (Generic 0 0)] (ArrayT $ Generic 0 0),
    -- (NumberT, [a]) -> BoolT
    Fn [NumberT, ArrayT (Generic 0 0)] BoolT,
    -- (k, a) -> Map<k, a>
    Fn [Generic 0 0, Generic 0 1] (HigherOrderN "Map" [Generic 0 0, Generic 0 1]),
    -- (k, a) -> Record<k, a>
    Fn [Generic 0 0, Generic 0 1] (HigherOrderN "Record" [Generic 0 0, Generic 0 1]),
    -- [[a]] -> [a]
    Fn [ArrayT (ArrayT $ Generic 0 0)] (ArrayT $ Generic 0 0),
    -- NumberT -> [a] -> [a]
    Fn [NumberT] $ Fn [ArrayT (Generic 1 0)] (ArrayT $ Generic 1 0),
    -- NumberT -> [a] -> [[a]]
    Fn [NumberT] $ Fn [ArrayT (Generic 1 0)] (ArrayT $ ArrayT $ Generic 1 0),
    -- (NumberT, (NumberT -> a)) -> [a] 
    Fn [NumberT, Fn [NumberT] (Generic 0 0)] (ArrayT $ Generic 0 0),
    -- (() -> a, () -> a) -> BoolT -> a
    Fn [Fn [] (Generic 0 0), Fn [] (Generic 0 0)] $ Fn [BoolT] (Generic 0 0),
    -- (a, NumberT) -> Any -> Any
    Fn [Generic 0 0, NumberT] $ Fn [Any] Any,
    -- (Record <k, Unknown>) -> [k]
    Fn [HigherOrderN "Record" [Generic 0 0,Unknown]] (ArrayT $ Generic 0 0),
    -- (Record <StringT, Unknown>) -> BoolT
    Fn [HigherOrderN "Record" [StringT,Unknown]] BoolT,
    -- (Record <StringT, Unknown>) -> NumberT
    Fn [HigherOrderN "Record" [StringT,Unknown]] NumberT,
    -- RegExp -> StringT -> Any
    Fn [Named "RegExp"] $ Fn [StringT] Any,
    -- RegExp -> Any
    Fn [Named "RegExp"] Any,
    -- (RegExp, StringT) -> StringT -> StringT
    Fn [Named "RegExp", StringT] $ Fn [StringT] StringT,
    -- Response -> Promise<Any>
    Fn [Named "Response"] (HigherOrder1 "Promise" Any),
    -- Response -> Promise<StringT>
    Fn [Named "Response"] (HigherOrder1 "Promise" StringT)
  ]
