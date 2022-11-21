{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DuplicateRecordFields #-}

module AOC2021.Day16.Main where

import Des (Bit(..), runDes, Dict(..), Des(..), getPosition)
import Deserialize (Deserialize(..), GenericInstance(..), evalDeserializer, bytify, bitsToNatural, des')
import Vector (Vector)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import Numeric.Natural (Natural)
import Data.Int (Int64)

import qualified Des.Do as Des
import Data.List (foldl')


hexToBit :: Char -> [Bit]
hexToBit c = case c of
    '0' -> [0, 0, 0, 0]
    '1' -> [0, 0, 0, 1]
    '2' -> [0, 0, 1, 0]
    '3' -> [0, 0, 1, 1]
    '4' -> [0, 1, 0, 0]
    '5' -> [0, 1, 0, 1]
    '6' -> [0, 1, 1, 0]
    '7' -> [0, 1, 1, 1]
    '8' -> [1, 0, 0, 0]
    '9' -> [1, 0, 0, 1]
    'A' -> [1, 0, 1, 0]
    'B' -> [1, 0, 1, 1]
    'C' -> [1, 1, 0, 0]
    'D' -> [1, 1, 0, 1]
    'E' -> [1, 1, 1, 0]
    'F' -> [1, 1, 1, 1]

hexToBits :: String -> [Bit]
hexToBits = concatMap hexToBit

data Header = MkHeader
    { version :: Vector Bit 3
    , typeId  :: Vector Bit 3
    }
    deriving stock (Show, Generic)
    deriving Deserialize via GenericInstance Header

data PrefixedLiteralValueGroup = MkPrefixedLiteralValueGroup
    { more  :: Bit
    , group :: Vector Bit 4
    }
    deriving stock (Show, Generic)
    deriving Deserialize via GenericInstance PrefixedLiteralValueGroup

newtype LiteralValue = MkLiteralValue
    { prefixedLiteralValueGroups :: [PrefixedLiteralValueGroup]
    }
    deriving stock (Show)
instance Deserialize LiteralValue where
    type instance Req LiteralValue = ()
    type instance Lrn LiteralValue = ()
    des = MkLiteralValue <$> go
      where
        go = do
            group@(MkPrefixedLiteralValueGroup p g) <- des' @PrefixedLiteralValueGroup
            if p == 0 then
                return [group]
            else do
                rest <- go
                return $ group : rest

subPacketsTotalLength :: Int64 -> Des () [Packet]
subPacketsTotalLength 0 = return []
subPacketsTotalLength bitsLeft = do
    pos <- getPosition
    packet <- des' @Packet
    pos' <- getPosition
    let bitsConsumed = pos' - pos
        bitsLeft' = bitsLeft - bitsConsumed
    packets <- subPacketsTotalLength bitsLeft'
    return $ packet : packets

subPacketsNumberOfSubPackets :: Int64 -> Des () [Packet]
subPacketsNumberOfSubPackets 0 = return []
subPacketsNumberOfSubPackets subPacketsLeft = do
    packet <- des' @Packet
    let subPacketsLeft' = subPacketsLeft - 1
    packets <- subPacketsNumberOfSubPackets subPacketsLeft'
    return $ packet : packets

data OperatorPacket = MkOperator
    { lengthTypeId :: Bit
    , length       :: Int64
    , subPackets  :: [Packet]
    }
    deriving stock (Show)
instance Deserialize OperatorPacket where
    type instance Req OperatorPacket = ()
    type instance Lrn OperatorPacket = ()
    des = do
        lengthTypeId <- des' @Bit
        if lengthTypeId == 0 then do
            length <- des' @(Vector Bit 15)
            let l = fromIntegral (bitsToNatural length)
            subPackets <- subPacketsTotalLength l
            return $ MkOperator lengthTypeId l subPackets
        else do
            length <- des' @(Vector Bit 11)
            let l = fromIntegral (bitsToNatural length)
            subPackets <- subPacketsNumberOfSubPackets l
            return $ MkOperator lengthTypeId l subPackets

type Operator = [Int] -> Int
data OperatorType = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo
    deriving (Show)

operator :: OperatorType -> Operator
operator op = case op of
    Sum -> sum
    Product -> product
    Minimum -> minimum
    Maximum -> maximum
    GreaterThan -> \operands -> let [x, y] = operands in fromEnum (x > y)
    LessThan -> \operands -> let [x, y] = operands in fromEnum (x < y)
    EqualTo -> \operands -> let [x, y] = operands in fromEnum (x == y)

data Body = LiteralValue LiteralValue | Operator OperatorType OperatorPacket
    deriving stock (Show)

data Packet = MkPacket
    { header :: Header
    , body   :: Body
    }
    deriving stock (Show)

instance Deserialize Packet where
    type instance Req Packet = ()
    type instance Lrn Packet = ()
    des = do
        header@(MkHeader version typeId) <- des' @Header
        let t = bitsToNatural typeId
        body <-
            if t == 4 then
                LiteralValue <$> des' @LiteralValue
            else
                let opType = case t of
                        0 -> Sum
                        1 -> Product
                        2 -> Minimum
                        3 -> Maximum
                        5 -> GreaterThan
                        6 -> LessThan
                        7 -> EqualTo
                in Operator opType <$> des' @OperatorPacket
        return $ MkPacket header body

sumVersions :: Packet -> Int
sumVersions (MkPacket (MkHeader version _) body) =
    fromIntegral (bitsToNatural version) +
        case body of
            LiteralValue _ -> 0
            Operator _ (MkOperator _ _ subPackets) ->
                sum $ map sumVersions subPackets

solve1 :: Packet -> Int
solve1 = sumVersions

literalValue :: LiteralValue -> Int
literalValue (MkLiteralValue prefixedGroups) =
    foldl' (\acc group -> acc * 16 + group) 0 $
        map (\(MkPrefixedLiteralValueGroup _ group) -> fromIntegral (bitsToNatural group)) prefixedGroups

eval :: Packet -> Int
eval (MkPacket (MkHeader _ _) body) =
    case body of
        LiteralValue lv -> literalValue lv
        Operator op (MkOperator _ _ subPackets) -> operator op $ map eval subPackets

solve2 :: Packet -> Int
solve2 = eval

main :: IO ()
main = do
    [line] <- lines <$> readFile "AOC2021/Day16/input.txt"
    let packet = evalDeserializer @Packet $ BS.pack $ bytify $ hexToBits line
    print $ solve1 packet
    print $ solve2 packet
