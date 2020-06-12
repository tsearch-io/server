{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsearch.Search
  ( find,
    fixtures,
  )
where

import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (isJust)
import Tsearch.Query

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
delta :: Int -> [a] -> [b] -> Bool
delta diff as bs = abs (length as - length bs) <= diff

(><) :: [a] -> [b] -> Bool
(><) = delta 1

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
    Fn [HigherOrderN "Record" [Generic 0 0, Unknown]] (ArrayT $ Generic 0 0),
    -- (Record <StringT, Unknown>) -> BoolT
    Fn [HigherOrderN "Record" [StringT, Unknown]] BoolT,
    -- (Record <StringT, Unknown>) -> NumberT
    Fn [HigherOrderN "Record" [StringT, Unknown]] NumberT,
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
